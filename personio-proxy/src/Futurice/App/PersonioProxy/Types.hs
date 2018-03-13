module Futurice.App.PersonioProxy.Types where

import Control.Concurrent.STM (TVar)
import Futurice.IdMap         (IdMap)
import Futurice.Prelude
import Prelude ()

import qualified Personio

-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

data Ctx = Ctx
    { ctxLogger              :: !Logger
    , ctxPersonio            :: !(TVar (IdMap Personio.Employee))
    , ctxPersonioValidations :: !(TVar [Personio.EmployeeValidation])
    }
