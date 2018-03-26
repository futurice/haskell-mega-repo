module Futurice.App.PlanMillSync.Ctx (Ctx (..)) where

import Futurice.Prelude
import Futurice.Servant (Cache)
import PlanMill.Worker  (Workers)
import Prelude ()

import Futurice.App.PlanMillSync.Config

data Ctx = Ctx
    { ctxConfig       :: !Config
    , ctxLogger       :: !Logger
    , ctxManager      :: !Manager
    , ctxCache        :: !Cache
    , ctxReadWorkers  :: !(Maybe Workers)
    , ctxWriteWorkers :: !Workers
    }
