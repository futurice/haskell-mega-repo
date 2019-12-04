{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Personio (
    -- * Actions
    personioEmployees,
    -- * Requests
    personioEmployeesR,
    evalPersonioReq,
    evalPersonioReqIO,
    evalPersonioQueryIO,
    -- * Testing
    testPersonioEmployees,
    testPersonioValidations,
    -- * Types
    module Control.Monad.Personio,
    module Personio.Request,
    module Personio.Types,
    module Personio.Query,
    -- * Auxiliary
    internSimpleEmployees,
    ) where

import Control.Monad.Http (runHttpT)
import Futurice.EnvConfig (getConfig')
import Futurice.Prelude
import Prelude ()

import Control.Monad.Personio
import Personio.Eval
import Personio.Query
import Personio.Request
import Personio.Types

-------------------------------------------------------------------------------
-- Requests
-------------------------------------------------------------------------------

personioEmployeesR :: PersonioReq [Employee]
personioEmployeesR = PersonioEmployees

-------------------------------------------------------------------------------
-- Actions
-------------------------------------------------------------------------------

-- | Get a list of all employees in personio.
--
-- See <url>.
--
personioEmployees :: MonadPersonio m => m [Employee]
personioEmployees = personio personioEmployeesR

evalPersonioReqIO
    :: Manager
    -> Logger
    -> Cfg
    -> PersonioReq a
    -> IO a
evalPersonioReqIO mgr lgr cfg req
    = runLogT "personio" lgr
    $ flip runHttpT mgr
    $ flip runReaderT cfg
    $ evalPersonioReq req

evalPersonioQueryIO
    :: Manager
    -> Logger
    -> Cfg
    -> Query a
    -> IO a
evalPersonioQueryIO mgr lgr cfg query
    = runLogT "personio" lgr
    $ flip runHttpT mgr
    $ flip runReaderT cfg
    $ evalPersonioQuery query

-------------------------------------------------------------------------------
-- Testing
-------------------------------------------------------------------------------

-- | Test personio employees.
--
-- Load default configuration from envvars. Use fresh manager and logger.
--
-- > testPersonioEmployees >>= traverse_ print
--
testPersonioEmployees :: IO [Employee]
testPersonioEmployees = do
    mgr <- newManager tlsManagerSettings
    withStderrLogger $ \lgr -> runLogT "personio-test" lgr $ do
        cfg <- getConfig' "REPL" configurePersonioCfg
        liftIO $ evalPersonioReqIO mgr lgr cfg PersonioEmployees

testPersonioValidations :: IO [EmployeeValidation]
testPersonioValidations = do
    mgr <- newManager tlsManagerSettings
    withStderrLogger $ \lgr -> runLogT "personio-test" lgr $ do
        cfg <- getConfig' "REPL" configurePersonioCfg
        liftIO $ evalPersonioReqIO mgr lgr cfg PersonioValidations
