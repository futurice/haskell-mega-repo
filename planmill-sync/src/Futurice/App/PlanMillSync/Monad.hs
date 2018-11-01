{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module Futurice.App.PlanMillSync.Monad (
    runIntegrations',
    ) where

import Futurice.Integrations
import Futurice.Integrations.Monad.StateSet (stateSetFUM, stateSetPersonio)
import Futurice.Prelude
import PlanMill.Queries.Haxl                (initDataSourceWorkers)
import PlanMill.Worker                      (Workers)
import Prelude ()

import qualified Haxl.Core as H

import Futurice.App.PlanMillSync.Config
import Futurice.App.PlanMillSync.Ctx

runIntegrations' :: Ctx -> Integrations '[ ServFUM, ServPE, ServPM ] a -> IO a
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
    -> IntegrationsConfig '[ ServFUM, ServPE, ServPM]
    -> Integrations '[ ServFUM, ServPE, ServPM ] a
    -> IO a
runIntegrations'' mgr lgr now readWorkers cfg m = do
    runIntegrationsWithHaxlStore now stateStore m
  where
    stateStore = case cfg of
        IntCfgFUM token burl (IntCfgPersonio reqPE (IntCfgPlanMill reqPM IntCfgEmpty)) ->
            stateSetFUM lgr mgr token burl $
            stateSetPersonio lgr mgr reqPE $
            stateSetPlanMill' $
            Tagged H.stateEmpty
          where
            stateSetPlanMill' = case readWorkers of
                Nothing -> stateSetPlanMill lgr mgr reqPM
                Just ws -> \(Tagged store) -> Tagged $
                    H.stateSet (initDataSourceWorkers ws) store
