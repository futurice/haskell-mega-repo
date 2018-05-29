module Futurice.App.PersonioProxy.Types where

import Control.Concurrent.STM (TVar)
import Futurice.IdMap         (IdMap)
import Futurice.Postgres      (Connection, HasPostgresPool (..), Pool)
import Futurice.Prelude
import Prelude ()

import qualified Personio

-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

data Ctx = Ctx
    { ctxLogger              :: !Logger
    , ctxPostgres            :: !(Pool Connection)
    , ctxPersonio            :: !(TVar (IdMap Personio.Employee))
    , ctxPersonioValidations :: !(TVar [Personio.EmployeeValidation])
    }

instance HasPostgresPool Ctx where
    postgresPool = ctxPostgres
