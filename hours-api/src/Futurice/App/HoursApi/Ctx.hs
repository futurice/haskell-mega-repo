{-# LANGUAGE DataKinds #-}
module Futurice.App.HoursApi.Ctx where

import Control.Concurrent.STM (TVar)
import Futurice.Cache         (Cache)
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified FUM.Types.Login as FUM
import qualified Personio        as P
import qualified PlanMill        as PM
import qualified PlanMill.Worker as PM

data Ctx = Ctx
    { ctxMockUser             :: !(Maybe FUM.Login)
    , ctxPersonioPlanmillMap  :: !(TVar (HashMap FUM.Login (P.Employee, PM.User)))
    , ctxCache                :: !Cache
    , ctxLogger               :: !Logger
    , ctxManager              :: !Manager
    , ctxWorkers              :: !PM.Workers
    , ctxPlanmillCfg          :: !PM.Cfg
    , ctxIntegrationsCfg      :: !(IntegrationsConfig '[I, Proxy, Proxy, Proxy, Proxy, I])
    }
