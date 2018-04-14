{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.PlanMillSync (defaultMain) where

import Futurice.Integrations
import Futurice.Lucid.Foundation (HtmlPage, fullRow_, h1_, page_)
import Futurice.Prelude
import Futurice.Servant
import PlanMill.Worker           (workers)
import Prelude ()
import Servant

import FUM.Types.Login         (Login)
import Futurice.FUM.MachineAPI (FUM6 (..), fum6)

import Futurice.App.PlanMillSync.Actions
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
    -- actions
    :<|> addDepartDateAction ctx
    :<|> updateStatusAction ctx
    :<|> updateContractTypeAction ctx

-------------------------------------------------------------------------------
-- Indexpage
-------------------------------------------------------------------------------

indexPageAction
    :: Ctx
    -> Maybe Login
    -> Handler (HtmlPage "index")
indexPageAction ctx mfu = withAuthorisedUser ctx mfu err $ do
    today <- currentDay

    -- fetch data
    (pm, p) <- liftIO $ cachedIO lgr cache 300 () $
        runIntegrations' ctx fetcher

    pure $ indexPage today pm p
  where
    err = pure page404

    lgr   = ctxLogger ctx
    cache = ctxCache ctx

page404 :: HtmlPage a
page404 = page_ "PlanMill Sync - Unauthorised" $
    fullRow_ $ do
        h1_ "Unauthorised"
        "Ask IT Team for access rights"

-------------------------------------------------------------------------------
-- Actions
-------------------------------------------------------------------------------

addDepartDateAction :: Ctx -> Maybe Login -> Login -> Handler (CommandResponse ())
addDepartDateAction ctx mfu login = withAuthorisedUser ctx mfu err $
    liftIO $ updateDepartDate ctx login
  where
    err = return (CommandResponseError "not authorised")

updateStatusAction :: Ctx -> Maybe Login -> Login -> Handler (CommandResponse ())
updateStatusAction ctx mfu login = withAuthorisedUser ctx mfu err $
    liftIO $ updateStatus ctx login
  where
    err = return (CommandResponseError "not authorised")

updateContractTypeAction :: Ctx -> Maybe Login -> Login -> Handler (CommandResponse ())
updateContractTypeAction ctx mfu login = withAuthorisedUser ctx mfu err $
    liftIO $ updateContractType ctx login
  where
    err = return (CommandResponseError "not authorised")

-------------------------------------------------------------------------------
-- Utils
-------------------------------------------------------------------------------

withAuthorisedUser :: Ctx -> Maybe Login -> Handler a -> Handler a -> Handler a
withAuthorisedUser ctx mfu err action = do
    now <- currentTime

    -- Access control: TODO cache?
    fus <- liftIO $ runIntegrations mgr lgr now (cfgIntegrationsConfig2 cfg) $
        fum6 $ FUMGroupEmployees (cfgAccessGroup cfg)

    case mfu <|> cfgMockUser cfg of
        Just fu | Set.member fu fus -> action

        -- TODO: log?
        _ -> err
  where
    cfg   = ctxConfig ctx
    lgr   = ctxLogger ctx
    mgr   = ctxManager ctx

type M = Integrations '[I, I, Proxy, Proxy, Proxy, I]

fetcher :: M ([PMUser], [P.Employee])
fetcher = liftA2 (,) users (P.personio P.PersonioEmployees)

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverService           .~ PlanmillSyncService
    & serverDescription       .~ "Sync people from personio to planmill"
    & serverApp planmillSyncApi .~ server
    & serverColour            .~  (Proxy :: Proxy ('FutuAccent 'AF1 'AC2))
    & serverEnvPfx            .~ "PLANMILLSYNC"
    & serverOpts              .~ optionsFlag False [(True, "planmill-direct"), (False, "planmill-proxy")] "Access PlanMill directly"
  where
    makeCtx :: Bool -> Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
    makeCtx planmillDirect cfg lgr mgr cache _mq = do
        ws <- workers lgr mgr (cfgPlanMillCfg cfg) ["worker1", "worker2", "worker3"]
        let readWorkers = if planmillDirect then Just ws else Nothing
        let ctx = Ctx cfg lgr mgr cache readWorkers ws
        pure (ctx, [])
