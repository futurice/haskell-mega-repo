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

import qualified Futurice.App.FUM.Types.ScheduleEmployee as S
import qualified Personio

machineServer :: Ctx -> Server FumCarbonMachineApi
machineServer ctx = machineServer' ctx
    :<|> personioRequest ctx
    :<|> rawEmployees ctx
    :<|> rawValidations ctx
    :<|> scheduleEmployees ctx

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

personioRequest :: Ctx -> Personio.SomePersonioReq -> Handler Personio.SomePersonioRes
personioRequest ctx (Personio.SomePersonioReq res) = case res of
    Personio.PersonioEmployees   -> Personio.SomePersonioRes res <$> rawEmployees ctx
    Personio.PersonioValidations -> Personio.SomePersonioRes res <$> rawValidations ctx
    Personio.PersonioAll         -> do
        es <- rawEmployees ctx
        vs <- rawValidations ctx
        pure (Personio.SomePersonioRes res (es, vs))

rawEmployees :: Ctx -> Handler [Personio.Employee]
rawEmployees ctx = do
    es <- liftIO $ readTVarIO $ ctxPersonio ctx
    -- no filtering, all employees
    pure $ toList es

rawValidations :: Ctx -> Handler [Personio.EmployeeValidation]
rawValidations ctx = liftIO $ readTVarIO $ ctxPersonioValidations ctx

scheduleEmployees :: Ctx -> Handler [S.ScheduleEmployee]
scheduleEmployees ctx = do
    es <- liftIO $ readTVarIO $ ctxPersonio ctx
    -- no filtering, all employees
    pure $ S.fromPersonio $ toList es
