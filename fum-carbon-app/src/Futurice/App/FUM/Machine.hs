{-# LANGUAGE GADTs             #-}
-- | Implementation of the machine API.
module Futurice.App.FUM.Machine (machineServer) where

import Control.Concurrent.STM (readTVarIO)
import Control.Monad.Reader   (Reader, asks, runReader)
import Data.Set.Lens          (setOf)
import Futurice.Prelude
import Futurice.Signed        (SecretKey, Signed, signed, sign)
import Prelude ()
import Servant

import Futurice.App.FUM.API
import Futurice.App.FUM.Config
import Futurice.App.FUM.Ctx
import Futurice.App.FUM.Types
import Futurice.FUM.MachineAPI

machineServer :: Ctx -> Server FumCarbonMachineApi
machineServer ctx = machineServer' ctx

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
    haxl' (FUMGroupEmployees n) = signed <$> eg n

    secretKey :: SecretKey
    secretKey = cfgSecretKey (ctxConfig ctx)

    eg :: GroupName -> Reader World (Signed (Set Login))
    eg name = asks
        $ sign [ secretKey ]
        . setOf (worldGroups . ix name . groupEmployees . folded)
