module Futurice.App.FlowdockProxy.Ctx where

import Control.Concurrent.STM (TVar)
import Futurice.Postgres      (Connection, HasPostgresPool (..), Pool)
import Futurice.Prelude
import Prelude ()

import qualified Chat.Flowdock.REST as FD

import Futurice.App.FlowdockProxy.Config (Config)
import Futurice.App.FlowdockProxy.DB     (Row)

data Ctx = Ctx
    { ctxConfig   :: !Config
    , ctxLogger   :: !Logger
    , ctxManager  :: !Manager
    , ctxPostgres :: !(Pool Connection)
    , ctxFlowOrg  :: !FD.Organisation
    , ctxFlowMap  :: !(TVar (Map FD.FlowId (Int, Text)))
    , ctxFlowRows :: !(TVar (Map FD.FlowId (TVar [Row])))
    }

instance HasPostgresPool Ctx where
    postgresPool = ctxPostgres
