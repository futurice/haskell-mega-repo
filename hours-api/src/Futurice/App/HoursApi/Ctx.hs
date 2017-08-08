module Futurice.App.HoursApi.Ctx where

import Control.Concurrent.STM (TVar)
import Futurice.Cache         (DynMapCache)
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified FUM
import qualified PlanMill        as PM
import qualified PlanMill.Worker as PM

data Ctx = Ctx
    { ctxMockUser        :: !(Maybe FUM.Login)
    , ctxFumPlanmillMap  :: !(TVar (HashMap FUM.Login (FUM.User, PM.User)))
    , ctxCache           :: !DynMapCache
    , ctxLogger          :: !Logger
    , ctxManager         :: !Manager
    , ctxWorkers         :: !PM.Workers
    , ctxPlanmillCfg     :: !PM.Cfg
    , ctxIntegrationsCfg :: !(IntegrationsConfig I I Proxy Proxy Proxy)
    }
