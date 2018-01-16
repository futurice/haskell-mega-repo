{-# LANGUAGE DataKinds #-}
module Futurice.App.HoursApi.Ctx where

import Control.Concurrent.STM (TVar)
import Futurice.Cache         (Cache)
import Futurice.Integrations
import Futurice.Postgres
import Futurice.Prelude
import Prelude ()

import qualified FUM
import qualified PlanMill        as PM
import qualified PlanMill.Worker as PM

data Ctx = Ctx
    { ctxMockUser        :: !(Maybe FUM.Login)
    , ctxFumPlanmillMap  :: !(TVar (HashMap FUM.Login (FUM.User, PM.User)))
    , ctxCache           :: !Cache
    , ctxLogger          :: !Logger
    , ctxManager         :: !Manager
    , ctxWorkers         :: !PM.Workers
    , ctxPlanmillCfg     :: !PM.Cfg
    , ctxIntegrationsCfg :: !(IntegrationsConfig '[I, I, Proxy, Proxy, Proxy, Proxy])
    , ctxPostgresPool    :: !(Pool Connection)
    }

instance HasPostgresPool Ctx where
  postgresPool = ctxPostgresPool
