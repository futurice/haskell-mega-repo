module Futurice.App.HC.Ctx where

import Futurice.Postgres
import Futurice.Prelude
import Prelude ()

import Futurice.App.HC.Config

data Ctx = Ctx
    { ctxConfig   :: !Config
    , ctxLogger   :: !Logger
    , ctxManager  :: !Manager
    , ctxSecret   :: !ByteString
    , ctxPostgres :: !(Pool Connection)
    }

instance HasPostgresPool Ctx where
    postgresPool = ctxPostgres
