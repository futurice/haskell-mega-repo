{-# LANGUAGE BangPatterns          #-}
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

import Control.Concurrent        (threadDelay)
import Control.Concurrent.Async  (async)
import Control.Concurrent.STM
       (atomically, newTVar, newTVarIO, readTVar, readTVarIO, writeTVar)
import Data.Aeson                (object, (.=))
import Data.Char                 (toLower)
import Data.Ord                  (comparing)
import Data.Time                 (addDays)
import FUM.Types.Login           (Login)
import Futurice.Integrations
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Periocron
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant
import Servant.Cached            (mkCached)
import Servant.Chart             (Chart)
import Servant.Server.Generic

import qualified Chat.Flowdock.REST    as FD
import qualified Data.List             as L
import qualified Data.Map.Strict       as Map
import qualified Data.Text             as T
import qualified Data.Text.Short       as TS
import qualified Futurice.Postgres     as PQ
import qualified Personio              as P
import qualified Servant.Types.SourceT as S

import Futurice.App.FlowdockProxy.API
import Futurice.App.FlowdockProxy.Charts
import Futurice.App.FlowdockProxy.Config
import Futurice.App.FlowdockProxy.Ctx
import Futurice.App.FlowdockProxy.DB
import Futurice.App.FlowdockProxy.IndexPage
import Futurice.App.FlowdockProxy.UsersPage

server :: Ctx -> Server FlowdockProxyAPI
server ctx = genericServer $ Record
    { recIndex = indexPageAction ctx
    , recUsers = usersPageAction ctx
    , recCharts = return chartsPage
    , recChartActivity
        = liftIO
        $ cachedIO (ctxLogger ctx) (ctxCache ctx) 600 ()
        $ fmap mkCached
        $ activityChartAction ctx

    -- API
    , recApiFlows    = apiFlowsAction ctx
    , recApiMessages = apiMessagesAction ctx
    }

-------------------------------------------------------------------------------
-- Activity chart
-------------------------------------------------------------------------------

activityChartAction :: Ctx ->  IO (Chart "activity")
activityChartAction ctx = do
    today <- currentDay
    let fromDay = addDays (-180) today

    (flowMap, allRows) <- atomically $ do
        flowMap <- readTVar (ctxFlowMap ctx)
        m <- readTVar (ctxFlowRows ctx)
        allRows <- traverse readTVar m
        return (flowMap, allRows)

    let xss :: Map Text (Map Int Int)
        xss = Map.fromList
            [ (flowName, xs)
            | (flowId, (_, flowName)) <- Map.toList flowMap
            , let rows = fromMaybe [] $ Map.lookup flowId allRows
            , let xs = Map.fromListWith (+)
                    [ (todHour (localTimeOfDay lt), 1)
                    | row <- rows
                    , let lt = utcToHelsinkiTime (rowCreatedAt row)
                    , fromDay < localDay lt
                    ]
            ]

    return $ activityChart xss

-------------------------------------------------------------------------------
-- Index
-------------------------------------------------------------------------------

indexPageAction
    :: Ctx
    -> Maybe Text  -- ^ user
    -> Maybe Text  -- ^ nick
    -> Maybe FD.FlowId
    -> Handler (HtmlPage "index-page")
indexPageAction ctx mneedle muserText mflow = do
    liftIO $ runLogT "index-page" (ctxLogger ctx) $ do
        flows <- liftIO (readTVarIO (ctxFlowMap ctx))
        case (,) <$> mneedle <*> mflow of
            Nothing ->
                return $ indexPage org flows Nothing muid Nothing []
            Just (needle', flow) | T.length needle' < 3 ->
                return $ indexPage org flows Nothing muid (Just flow) []

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
    muid = muserText >>= \x -> Map.lookup (T.toLower x) users

    strong :: [(FD.FlowId, [Row])] -> [[(FD.FlowId, Row)]]
    strong = map (\(flowId, rows) -> map (flowId,) rows)

    -- lower case nick to uid
    users :: Map Text FD.UserId
    users = Map.fromList
        [ (orgUser ^. FD.userNick . getter T.toLower, orgUser ^. FD.userId)
        | orgUser <- org ^.. FD.orgUsers . folded
        ]

    search
        :: Text
        -> Map FD.FlowId (Int, Text)
        -> Maybe FD.FlowId
        -> [(FD.FlowId, Row)]
        -> LogT IO (HtmlPage "index-page")
    search needle' flows mflow' allRows = do
        let needle = map toLower (T.unpack needle')
        let rows = take 1000
                [ row
                | row <- allRows
                , let row' = snd row
                , needle `L.isInfixOf` map toLower (TS.toString $ rowText row')
                , maybe True (\uid -> uid == rowUser row') muid
                ]

        return $ indexPage org flows (Just needle') muid mflow' rows

    merge []       = []
    merge [xs]     = xs
    merge (xs:xss) = merge2 xs (merge xss)

    merge2 [] ys = ys
    merge2 xs [] = xs
    merge2 xs@(x : xs') ys@(y : ys') = case comparing (rowCreatedAt . snd) x y of
        LT -> y : merge2 xs ys'
        GT -> x : merge2 xs' ys
        EQ -> x : y : merge2 xs' ys'

-------------------------------------------------------------------------------
-- Users
-------------------------------------------------------------------------------

usersPageAction :: Ctx -> Maybe Login -> Handler (HtmlPage "users-page")
usersPageAction ctx _ = do
    extraOrgs' <- runLogT "users" lgr $ PQ.safePoolQuery_ ctx
        "SELECT organisation_name FROM \"flowdock-proxy\".organisations ORDER BY organisation_name ASC;"
    let extraOrgs :: [FD.ParamName FD.Organisation]
        extraOrgs = map (FD.mkParamName . fromOnly) extraOrgs'
    (today, ps, orgs) <- liftIO $ cachedIO lgr cch 600 () $ runIntegrations' ctx $ do
        today <- currentDay
        ps <- personio P.PersonioEmployees
        orgs <- traverse flowdockOrganisationReq extraOrgs
        return (today, ps, orgs)
    return $ usersPage today (ctxFlowOrg ctx) orgs ps
  where
    lgr = ctxLogger ctx
    cch = ctxCache ctx

-------------------------------------------------------------------------------
-- API: flows
-------------------------------------------------------------------------------

apiFlowsAction :: Ctx -> Handler [FD.FlowId]
apiFlowsAction ctx = do
    flows <- liftIO (readTVarIO (ctxFlowMap ctx))
    return $ Map.keys flows

apiMessagesAction :: Ctx -> FD.FlowId -> Handler (S.SourceT IO Row)
apiMessagesAction ctx flowName = do
    mrows <- liftIO (readTVarIO (ctxFlowRows ctx))
    case mrows ^? ix flowName of
        Nothing       -> throwError err404
        Just rowsTVar -> do
            rows <- liftIO (readTVarIO rowsTVar)
            return (S.source rows)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverService              .~ FlowdockProxyService
    & serverDescription          .~ "Flowdock Proxy"
    & serverApp flowdockProxyApi .~ server
    & serverColour               .~ (Proxy :: Proxy ('FutuAccent 'AF3 'AC2))
    & serverEnvPfx               .~ "FLOWDOCK_PROXY"
  where
    makeCtx :: () -> Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
    makeCtx () cfg lgr mgr cache _mq = do
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
        let ctx = Ctx cfg lgr mgr cache pool flowOrg flowMapTVar flowRowsTVar

        -- Read contents into the memory immediately
        void $ async $ runLogT "initial-read" lgr $ do
            ifor_ flowMap $ \flowSlug (flowId, _) -> do
                logInfoI "Initial read $flow" $ object [ "flow" .= flowSlug ]
                readRows ctx flowId flowSlug

        -- Update job: fetch new rows
        let updateJobAction = runLogT "update-rows" lgr $
                ifor_ flowMap $ \flowSlug (flowId, _) -> do
                    liftIO $ threadDelay $ 5*1000000  -- sleeping between each flow
                    logInfoI "Updating flow $flow" $ object [ "flow" .= flowSlug ]
                    updateRows ctx flowId flowSlug

        let updateJob = mkJob "update-flows" updateJobAction $ every 1800

        pure (ctx, [updateJob])

-------------------------------------------------------------------------------
-- Update rows
-------------------------------------------------------------------------------

readRows :: Ctx -> Int -> FD.FlowId -> LogT IO ()
readRows ctx flowId flowSlug = do
    rows <- queryRows ctx flowId
    liftIO $ atomically $ do
        m <- readTVar (ctxFlowRows ctx)
        case Map.lookup flowSlug m of
            Nothing -> do
                tvar <- newTVar rows
                writeTVar (ctxFlowRows ctx) (Map.insert flowSlug tvar m)
            Just tvar -> do
                writeTVar tvar rows

updateRows :: Ctx -> Int -> FD.FlowId -> LogT IO ()
updateRows ctx flowId flowSlug = do
    -- join :: Maybe (Maybe FD.MessageId) -> Maybe FD.MessageId to allow selecting NULL
    messageId <- join . listToMaybe . map fromOnly <$> PQ.safePoolQuery ctx
        "SELECT MAX(message_id) from \"flowdock-proxy\".messages where flow_id = ?"
        (Only flowId)

    logInfoI "updating flow $flow" $ object [ "flow" .= flowId, "messageId" .= (messageId :: Maybe FD.MessageId) ]

    -- fetch stuff
    new <- loop False messageId

    -- cache in memory
    when new $ readRows ctx flowId flowSlug
  where
    org = ctxFlowOrg ctx

    loop :: Bool -> Maybe FD.MessageId -> LogT IO Bool
    loop !acc messageId = do
        logInfoI "Messages in $flow since $since" $ object [ "flow" .= flowSlug, "since" .= messageId ]

        messages <- liftIO $ runIntegrations' ctx $ flowdockMessagesSinceReq
            (org ^. FD.orgParamName)
            (FD.mkParamName $ FD.getIdentifier flowSlug)
            messageId

        if null messages
        then return acc
        else do
            insertRows ctx flowId $ mapMaybe messageToRow $ toList messages
            loop True (Just $ maximum $ messages ^.. folded . FD.msgId)

-------------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------------

runIntegrations' :: Ctx -> Integrations '[ ServFD, ServFUM6, ServPE] a -> IO a
runIntegrations' ctx m = do
    -- ! New manager each run. Maybe this helps with timeouts, maybe not.
    mgr <- newManager tlsManagerSettings
    now <- currentTime
    runIntegrations mgr lgr now (cfgIntegrationsConfig cfg) m
  where
    -- mgr = ctxManager ctx
    lgr = ctxLogger ctx
    cfg = ctxConfig ctx
