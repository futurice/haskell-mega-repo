{-# LANGUAGE DataKinds #-}
module Futurice.App.PlanMillSync.Monad (
    runIntegrations',
    ) where

import Control.Category      ((>>>))
import Futurice.Integrations
import Futurice.Prelude
import PlanMill.Queries.Haxl (initDataSourceWorkers)
import PlanMill.Worker       (Workers)
import Prelude ()

import qualified Haxl.Core as H

import Futurice.App.PlanMillSync.Config
import Futurice.App.PlanMillSync.Ctx

runIntegrations' :: Ctx -> Integrations [I, I, Proxy, Proxy, Proxy, I] a -> IO a
runIntegrations' ctx m = do
    now <- currentTime
    runIntegrations''
        (ctxManager ctx)
        (ctxLogger ctx)
        now
        (ctxReadWorkers ctx)
        (cfgIntegrationsConfig (ctxConfig ctx))
        m

runIntegrations''
    :: Manager -> Logger -> UTCTime
    -> Maybe Workers  -- ^ read workers
    -> IntegrationsConfig '[I, I, Proxy, Proxy, Proxy, I]
    -> Integrations [I, I, Proxy, Proxy, Proxy, I] a
    -> IO a
runIntegrations'' mgr lgr now readWorkers cfg m = do
    runIntegrationsWithHaxlStore now stateMorphism cfg m
  where
    stateMorphism = morph
        >>> fumIntegrationStateMorphism lgr mgr cfg
        >>> peIntegrationStateMorphism lgr mgr cfg

    morph' = pmIntegrationStateMorphism lgr mgr cfg
    morph  = mcase readWorkers morph' $ \workers ->
        IntegrSM $ H.stateSet $ initDataSourceWorkers workers
