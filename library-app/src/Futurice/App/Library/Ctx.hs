module Futurice.App.Library.Ctx (
    Ctx (..)
    ) where

import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant  (Cache)
import Prelude ()

import Futurice.App.Library.Config

data Ctx = Ctx
    { ctxConfig   :: !Config
    , ctxPostgres :: !(Pool Connection)
    , ctxLogger   :: !Logger
    , ctxManager  :: !Manager
    , ctxCache    :: !Cache
    }

instance HasPostgresPool Ctx where
    postgresPool = ctxPostgres
