{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.PlanMillSync (defaultMain) where

import Futurice.Integrations
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Lucid.Foundation (fullRow_, h1_, page_)
import Futurice.Prelude
import Futurice.Servant
import PlanMill.Worker           (workers)
import Prelude ()
import Servant

import FUM.Types.Login         (Login)
import Futurice.FUM.MachineAPI (FUM6 (..), fum6)

import Futurice.App.PlanMillSync.API
import Futurice.App.PlanMillSync.Config
import Futurice.App.PlanMillSync.Ctx
import Futurice.App.PlanMillSync.IndexPage
import Futurice.App.PlanMillSync.Monad
import Futurice.App.PlanMillSync.Types

import qualified Data.Set as Set
import qualified Personio as P

server :: Ctx -> Server PlanMillSyncAPI
server ctx = indexPageAction ctx
    -- TODO: actions to add & remove

indexPageAction
    :: Ctx
    -> Maybe Login
    -> Handler (HtmlPage "index")
indexPageAction ctx mfu = do
    now <- currentTime

    -- Access control
    fus <- liftIO $ runIntegrations mgr lgr now (cfgIntegrationsConfig2 cfg) $
        fum6 $ FUMGroupEmployees (cfgAccessGroup cfg)
    liftIO $ print fus

    case mfu <|> cfgMockUser cfg of
        Just fu | Set.member fu fus -> do
            -- Data fetch
            (today, (pm, p)) <- liftIO $ cachedIO lgr cache 300 () $
                runIntegrations' mgr lgr now ws (cfgIntegrationsConfig cfg) $
                    liftA2 (,) currentDay fetcher

            -- Render
            pure $ indexPage today pm p

        _ -> pure page404 -- TODO: log unauhtorised access?
  where
    cfg   = ctxConfig ctx
    lgr   = ctxLogger ctx
    mgr   = ctxManager ctx
    ws    = ctxWorkers ctx
    cache = ctxCache ctx

page404 :: HtmlPage a
page404 = page_ "PlanMill Sync - Unauthorised" $
    fullRow_ $ do
        h1_ "Unauthorised"
        "Ask IT Team for access rights"

type M = Integrations '[I, I, Proxy, Proxy, Proxy, I]

fetcher :: M ([PMUser], [P.Employee])
fetcher = liftA2 (,) users (P.personio P.PersonioEmployees)

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName              .~ "PlanMill Sync"
    & serverDescription       .~ "Sync people from personio to planmill"
    & serverApp githubSyncApi .~ server
    & serverColour            .~  (Proxy :: Proxy ('FutuAccent 'AF5 'AC1))
    & serverEnvPfx            .~ "PLANMILLSYNC"
    & serverOpts              .~ optionsFlag False [(True, "planmill-direct"), (False, "planmill-proxy")] "Access PlanMill directly"
  where
    makeCtx :: Bool -> Config -> Logger -> Manager -> Cache -> IO (Ctx, [Job])
    makeCtx planmillDirect cfg lgr mgr cache = do
        ws <-
            if planmillDirect
            then Just <$> workers lgr mgr (cfgPlanMillCfg cfg) ["worker1", "worker2", "worker3"]
            else pure Nothing
        let ctx = Ctx cfg lgr mgr cache ws
        pure (ctx, [])
