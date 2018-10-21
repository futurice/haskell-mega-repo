{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.FlowdockProxy.Main (defaultMain) where

import Control.Concurrent.STM
       (atomically, newTVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Data.Aeson                (object, (.=))
import Data.Ord                  (comparing)
import Futurice.Integrations
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Periocron
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant
import Servant.Server.Generic

import qualified Chat.Flowdock.REST as FD
import qualified Data.Map.Strict    as Map
import qualified Data.Text          as T
import qualified Futurice.Postgres  as PQ

import Futurice.App.FlowdockProxy.API
import Futurice.App.FlowdockProxy.Config
import Futurice.App.FlowdockProxy.Ctx
import Futurice.App.FlowdockProxy.DB
import Futurice.App.FlowdockProxy.IndexPage

server :: Ctx -> Server FlowdockProxyAPI
server ctx = genericServer $ Record
    { recIndex = indexPageAction ctx
    }

indexPageAction
    :: Ctx
    -> Maybe Text
    -> Maybe FD.FlowId
    -> Handler (HtmlPage "index-page")
indexPageAction ctx mneedle mflow = do
    liftIO $ runLogT "index-page" (ctxLogger ctx) $ do
        flows <- liftIO (readTVarIO (ctxFlowMap ctx))
        case (,) <$> mneedle <*> mflow of
            Nothing ->
                return $ indexPage org flows Nothing Nothing []
            Just (needle', flow) | T.length needle' < 4 ->
                return $ indexPage org flows Nothing (Just flow) []

            -- special flow: "all". this is a HACK
            Just (needle', flow) | flow == FD.mkIdentifier "all" -> do
                allRows <- liftIO $ atomically $ do
                    m <- readTVar (ctxFlowRows ctx)
                    merge . strong . Map.toList <$> traverse readTVar m

                search needle' flows Nothing allRows

            Just (needle', flow) -> do
                allRows <- liftIO $ atomically $ do
                    m <- readTVar (ctxFlowRows ctx)
                    maybe (pure []) readTVar $ Map.lookup flow m

                search needle' flows (Just flow) $ map (flow,) allRows
  where
    org = ctxFlowOrg ctx

    strong :: [(FD.FlowId, [Row])] -> [[(FD.FlowId, Row)]]
    strong = map (\(flowId, rows) -> map (flowId,) rows)

    search needle' flows mflow' allRows = do
        let needle = T.toLower needle'
        let rows = take 1000
                [ row
                | row <- allRows
                , needle `T.isInfixOf` T.toLower (rowText (snd row))
                ]

        return $ indexPage org flows (Just needle) mflow' rows

    merge []       = []
    merge [xs]     = xs
    merge (xs:xss) = merge2 xs (merge xss)

    merge2 [] ys = ys
    merge2 xs [] = xs
    merge2 xs@(x : xs') ys@(y : ys') = case comparing (rowCreatedAt . snd) x y of
        LT -> y : merge2 xs ys'
        GT -> x : merge2 xs' ys
        EQ -> x : y : merge2 xs' ys'

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverService              .~ FlowdockProxyService
    & serverDescription          .~ "Flowdock Proxy"
    & serverApp flowdockProxyApi .~ server
    & serverColour               .~ (Proxy :: Proxy ('FutuAccent 'AF3 'AC2))
    & serverEnvPfx               .~ "FLOWDOCK_PROXY"
  where
    makeCtx :: () -> Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
    makeCtx () cfg lgr mgr _cache _mq = do
        pool <- PQ.createPostgresPool (cfgPostgresConnInfo cfg)

        -- flow map is reloaded on start
        flowMap <- runLogT "flows" lgr $ do
            flows <- PQ.safePoolQuery_ pool
                "SELECT flow_slug, flow_id, flow_name FROM \"flowdock-proxy\".flows;"
            return $ Map.fromList
                [ (slug, (fid, fname))
                | (slug, fid, fname) <- flows
                ]

        flowMapTVar <- newTVarIO flowMap

        -- organisation is fetched on start
        now <- currentTime
        flowOrg <- liftIO $ runIntegrations mgr lgr now (cfgIntegrationsConfig cfg) flowdockOrganisation

        -- Context
        flowRowsTVar <- newTVarIO mempty
        let ctx = Ctx cfg lgr mgr pool flowOrg flowMapTVar flowRowsTVar

        -- Update job: fetch new rows
        let updateJobAction = runLogT "update-rows" lgr $
                ifor_ flowMap $ \flowSlug (flowId, _) -> do
                    logInfoI "Updating flow $flow" $ object [ "flow" .= flowSlug ]
                    updateRows ctx flowId flowSlug

        let updateJob = mkJob "update-flows" updateJobAction $ every 1800

        pure (ctx, [updateJob])

-------------------------------------------------------------------------------
-- Update rows
-------------------------------------------------------------------------------

updateRows :: Ctx -> Int -> FD.FlowId -> LogT IO ()
updateRows ctx flowId flowSlug = do
    -- join :: Maybe (Maybe FD.MessageId) -> Maybe FD.MessageId to allow selecting NULL
    messageId <- join . listToMaybe . map PQ.fromOnly <$> PQ.safePoolQuery ctx
        "SELECT MAX(message_id) from \"flowdock-proxy\".messages where flow_id = ?"
        (PQ.Only flowId)

    logInfoI "updating flow $flow" $ object [ "flow" .= flowId, "messageId" .= (messageId :: Maybe FD.MessageId) ]

    -- fetch stuff
    loop messageId

    -- cache in memory
    rows <- queryRows ctx flowId
    liftIO $ atomically $ do
        m <- readTVar (ctxFlowRows ctx)
        case Map.lookup flowSlug m of
            Nothing -> do
                tvar <- newTVar rows
                writeTVar (ctxFlowRows ctx) (Map.insert flowSlug tvar m)
            Just tvar -> do
                writeTVar tvar rows
  where
    org = ctxFlowOrg ctx

    loop :: Maybe FD.MessageId -> LogT IO ()
    loop messageId = do
        logInfoI "Messages in $flow since $since" $ object [ "flow" .= flowSlug, "since" .= messageId ]

        messages <- liftIO $ runIntegrations' ctx $ flowdockMessagesSinceReq
            (org ^. FD.orgParamName)
            (FD.mkParamName $ FD.getIdentifier flowSlug)
            messageId

        if null messages
        then return ()
        else do
            insertRows ctx flowId $ mapMaybe messageToRow $ toList messages
            loop (Just $ maximum $ messages ^.. folded . FD.msgId)

-------------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------------

runIntegrations' :: Ctx -> Integrations '[Proxy, Proxy, Proxy, Proxy, I, Proxy] a -> IO a
runIntegrations' ctx m = do
    now <- currentTime
    runIntegrations mgr lgr now (cfgIntegrationsConfig cfg) m
  where
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    cfg = ctxConfig ctx
