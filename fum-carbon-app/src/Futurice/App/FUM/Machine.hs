{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Implementation of the machine API.
module Futurice.App.FUM.Machine (machineServer) where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad.Reader   (Reader, asks, runReader)
import Data.Set.Lens          (setOf)
import Futurice.Prelude
import Prelude ()
import Servant

import Futurice.App.FUM.API
import Futurice.App.FUM.Ctx
import Futurice.App.FUM.Types
import Futurice.FUM.MachineAPI

import qualified Personio as P

machineServer :: Ctx -> Server FumCarbonMachineApi
machineServer ctx = machineServer' ctx
    :<|> personioRequest ctx
    :<|> rawEmployees ctx
    :<|> rawValidations ctx

machineServer' :: Ctx -> Server FUMMachineAPI
machineServer' ctx = hoistServer fumMachineApi nt $ traverse haxl
    :<|> eg
  where
    nt :: Reader World a -> Handler a
    nt m = liftIO $ do
        w <- readTVarIO (ctxWorld ctx)
        return (runReader m w)

    haxl :: SomeFUM6 -> Reader World SomeFUM6Response
    haxl (SomeFUM6 req) = SomeFUM6Response req <$> haxl' req

    haxl' :: FUM6 a -> Reader World a
    haxl' (FUMGroupEmployees n) = eg n

    eg :: GroupName -> Reader World (Set Login)
    eg name = asks (setOf (worldGroups . ix name . groupEmployees . folded))

personioRequest :: Ctx -> P.SomePersonioReq -> Handler P.SomePersonioRes
personioRequest ctx (P.SomePersonioReq res) = case res of
    P.PersonioEmployees       -> P.SomePersonioRes res <$> rawEmployees ctx
    P.PersonioValidations     -> P.SomePersonioRes res <$> rawValidations ctx
    P.PersonioSimpleEmployees -> P.SomePersonioRes res <$> rawSimpleEmployees ctx
    P.PersonioAll             -> do
        es <- rawEmployees ctx
        vs <- rawValidations ctx
        pure (P.SomePersonioRes res (es, vs))

rawEmployees :: Ctx -> Handler [P.Employee]
rawEmployees ctx = do
    es <- liftIO $ readTVarIO $ ctxPersonio ctx
    -- no filtering, all employees
    pure $ toList es

rawValidations :: Ctx -> Handler [P.EmployeeValidation]
rawValidations ctx = liftIO $ readTVarIO $ ctxPersonioValidations ctx

rawSimpleEmployees :: Ctx -> Handler (Map Day [P.SimpleEmployee])
rawSimpleEmployees _ = fail "Not supported"
