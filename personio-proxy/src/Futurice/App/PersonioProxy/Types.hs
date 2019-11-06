module Futurice.App.PersonioProxy.Types where

import Control.Concurrent.STM (TVar)
import Futurice.Cache         (Cache)
import Futurice.Postgres      (Connection, HasPostgresPool (..), Pool)
import Futurice.Prelude
import Prelude ()

import qualified Personio

-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

data Ctx = Ctx
    { ctxLogger          :: !Logger
    , ctxCache           :: !Cache
    , ctxManager         :: !Manager
    , ctxConfig          :: !Personio.Cfg
    , ctxPostgres        :: !(Pool Connection)
    , ctxPersonioData    :: !(TVar Personio.PersonioAllData)
    }

instance HasPostgresPool Ctx where
    postgresPool = ctxPostgres
