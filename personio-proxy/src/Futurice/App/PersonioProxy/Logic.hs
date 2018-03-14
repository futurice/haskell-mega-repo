{-# LANGUAGE GADTs #-}
module Futurice.App.PersonioProxy.Logic where

import Control.Concurrent.STM           (readTVarIO)
import Futurice.App.PersonioProxy.Types
import Futurice.Prelude
import Prelude ()
import Servant

import qualified Personio

personioRequest :: Ctx -> Personio.SomePersonioReq -> Handler Personio.SomePersonioRes
personioRequest ctx (Personio.SomePersonioReq res) = case res of
    Personio.PersonioEmployees   -> Personio.SomePersonioRes res <$> rawEmployees ctx
    Personio.PersonioValidations -> Personio.SomePersonioRes res <$> rawValidations ctx
    Personio.PersonioAll         -> do
        es <- rawEmployees ctx
        vs <- rawValidations ctx
        pure (Personio.SomePersonioRes res (es, vs))

rawValidations :: Ctx -> Handler [Personio.EmployeeValidation]
rawValidations ctx = liftIO $ readTVarIO $ ctxPersonioValidations ctx

rawEmployees :: Ctx -> Handler [Personio.Employee]
rawEmployees ctx = do
    es <- liftIO $ readTVarIO $ ctxPersonio ctx
    -- no filtering, all employees
    pure $ toList es
