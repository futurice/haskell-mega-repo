module Futurice.App.PlanMillSync.Monad (
    runIntegrations',
    ) where

import Futurice.Integrations
import Futurice.Prelude
import PlanMill.Queries.Haxl (initDataSourceBatch, initDataSourceWorkers)
import PlanMill.Worker       (Workers)
import Prelude ()

import qualified FUM.Haxl
import qualified Haxl.Core     as H
import qualified Personio.Haxl
import qualified PlanMill      as PM

runIntegrations'
    :: Manager -> Logger -> UTCTime
    -> Maybe Workers
    -> IntegrationsConfig I I Proxy Proxy Proxy I
    -> Integrations I I Proxy Proxy Proxy I a
    -> IO a
runIntegrations' mgr lgr now mworkers cfg m = do
    runIntegrationsWithHaxlStore stateStore now cfg m
  where
    stateStore
        = pmStateSet
        . fumStateSet
        . peStateSet
        $ H.stateEmpty

    fumStateSet = unI $ fmap H.stateSet $ FUM.Haxl.initDataSource' mgr
        <$> integrCfgFumAuthToken cfg
        <*> integrCfgFumBaseUrl cfg
    peStateSet  = unI $ fmap H.stateSet $ Personio.Haxl.initDataSource lgr mgr
        <$> integrCfgPersonioProxyBaseRequest cfg

    pmStateSet'  = unI $ fmap H.stateSet $ initDataSourceBatch lgr mgr
        <$> integrCfgPlanmillProxyBaseRequest cfg
    pmStateSet  = mcase mworkers pmStateSet' $ \workers ->
        H.stateSet $ initDataSourceWorkers workers
