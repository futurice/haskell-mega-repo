module Futurice.App.Library.Ctx (
    Ctx (..)
    ) where

import Control.Concurrent.STM (TVar)
import Futurice.IdMap         (IdMap)
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant       (Cache)
import Prelude ()

import Futurice.App.Library.Config

import qualified Personio as P

data Ctx = Ctx
    { ctxConfig   :: !Config
    , ctxPostgres :: !(Pool Connection)
    , ctxLogger   :: !Logger
    , ctxManager  :: !Manager
    , ctxCache    :: !Cache
    , ctxPersonio :: !(TVar (IdMap P.Employee))
    }

instance HasPostgresPool Ctx where
    postgresPool = ctxPostgres
