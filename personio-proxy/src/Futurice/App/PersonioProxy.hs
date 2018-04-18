{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.PersonioProxy (defaultMain) where

import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar)
import Futurice.IdMap         (IdMap)
import Futurice.Periocron
import Futurice.Prelude
import Futurice.Servant
import Futurice.Time
import Prelude ()
import Servant                ((:<|>) (..), Server)

import Futurice.App.PersonioProxy.API
import Futurice.App.PersonioProxy.Config
import Futurice.App.PersonioProxy.Logic
import Futurice.App.PersonioProxy.Types

import qualified Data.Map.Strict as Map
import qualified Futurice.IdMap  as IdMap
import qualified Personio

server :: Ctx -> Server PersonioProxyAPI
server ctx = pure "Try /swagger-ui/"
    :<|> personioRequest ctx
    :<|> rawEmployees ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService       .~ PersonioProxyService
    & serverDescription   .~ "Make faster queries to Personio"
    & serverColour        .~ (Proxy :: Proxy ('FutuAccent 'AF6 'AC1))
    & serverApp personioProxyApi .~ server
    & serverEnvPfx        .~ "PERSONIOPROXY"

newCtx
    :: Logger
    -> IdMap Personio.Employee
    -> [Personio.EmployeeValidation]
    -> IO Ctx
newCtx lgr es vs = do
    Ctx lgr <$> newTVarIO es <*> newTVarIO vs

makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
makeCtx (Config cfg intervalMin) lgr mgr _cache mq = do
    -- employees
    let fetchEmployees = Personio.evalPersonioReqIO mgr lgr cfg Personio.PersonioAll
    (employees, validations) <- fetchEmployees

    -- context
    ctx <- newCtx lgr (IdMap.fromFoldable employees) validations

    -- jobs
    let intervalSec = unNDT (ndtConvert' intervalMin :: NDT 'Seconds NominalDiffTime) -- TODO: add this to futurice-prelude
    let employeesJob   = mkJob "Update personio data" (updateJob ctx fetchEmployees) $ tail $ every intervalSec

    pure (ctx, [ employeesJob ])
  where
    updateJob :: Ctx -> IO ([Personio.Employee], [Personio.EmployeeValidation]) -> IO ()
    updateJob ctx fetchEmployees = do
        (employees, validations) <- fetchEmployees
        changed <- atomically $ do
            oldMap <- readTVar (ctxPersonio ctx)
            let newMap = IdMap.fromFoldable employees
            writeTVar (ctxPersonio ctx) newMap
            writeTVar (ctxPersonioValidations ctx) validations
            return (comparePersonio oldMap newMap)

        unless (null changed) $ do
            runLogT "updated" (ctxLogger ctx) $ do
                logInfo "Personio updated, data changed" changed
                liftIO $ publishMessage mq PersonioUpdated

comparePersonio
    :: IdMap Personio.Employee                    -- ^ old
    -> IdMap Personio.Employee                    -- ^ new
    -> Map Personio.EmployeeId (These Personio.Employee Personio.Employee) -- ^ "diff"
comparePersonio old new =
    Map.mapMaybe id $ alignWith f (IdMap.toMap old) (IdMap.toMap new)
  where
    f x = case x of
        These o n | o == n -> Nothing
        _                  -> Just x
