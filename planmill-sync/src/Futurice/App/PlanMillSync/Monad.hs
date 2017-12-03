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

runIntegrations'
    :: Manager -> Logger -> UTCTime
    -> Maybe Workers
    -> IntegrationsConfig '[I, I, Proxy, Proxy, Proxy, I]
    -> Integrations [I, I, Proxy, Proxy, Proxy, I] a
    -> IO a
runIntegrations' mgr lgr now mworkers cfg m = do
    runIntegrationsWithHaxlStore now stateMorphism cfg m
  where
    stateMorphism = morph
        >>> fumIntegrationStateMorphism lgr mgr cfg
        >>> peIntegrationStateMorphism lgr mgr cfg

    morph' = pmIntegrationStateMorphism lgr mgr cfg
    morph  = mcase mworkers morph' $ \workers ->
        IntegrSM $ H.stateSet $ initDataSourceWorkers workers
