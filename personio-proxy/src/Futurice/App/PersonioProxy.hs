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
import qualified Personio        as P

server :: Ctx -> Server PersonioProxyAPI
server ctx = pure "Try /swagger-ui/"
    :<|> personioRequest ctx
    :<|> rawEmployees ctx
    :<|> scheduleEmployees ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService       .~ PersonioProxyService
    & serverDescription   .~ "Make faster queries to Personio"
    & serverColour        .~ (Proxy :: Proxy ('FutuAccent 'AF6 'AC1))
    & serverApp personioProxyApi .~ server
    & serverEnvPfx        .~ "PERSONIOPROXY"

newCtx
    :: Logger
    -> IdMap P.Employee
    -> [P.EmployeeValidation]
    -> IO Ctx
newCtx lgr es vs = do
    Ctx lgr <$> newTVarIO es <*> newTVarIO vs

makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
makeCtx (Config cfg intervalMin) lgr mgr _cache mq = do
    -- employees
    let fetchEmployees = P.evalPersonioReqIO mgr lgr cfg P.PersonioAll
    (employees', validations') <- fetchEmployees

    let employees = filter notMachine employees'
    let validations = filter (notMachine . view P.evEmployee) validations'

    -- context
    ctx <- newCtx lgr (IdMap.fromFoldable employees) validations

    -- jobs
    let intervalSec = unNDT (ndtConvert' intervalMin :: NDT 'Seconds NominalDiffTime) -- TODO: add this to futurice-prelude
    let employeesJob   = mkJob "Update personio data" (updateJob ctx fetchEmployees) $ tail $ every intervalSec

    pure (ctx, [ employeesJob ])
  where
    updateJob :: Ctx -> IO ([P.Employee], [P.EmployeeValidation]) -> IO ()
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

notMachine :: P.Employee -> Bool
notMachine e = case e ^. P.employeeId of
    -- hardcoded here, these are ids of emailer accounts. they are no humans.
    P.EmployeeId 331863 -> False
    P.EmployeeId 386126 -> False
    P.EmployeeId 590516 -> False
    P.EmployeeId 656474 -> False
    _      -> True

comparePersonio
    :: IdMap P.Employee                    -- ^ old
    -> IdMap P.Employee                    -- ^ new
    -> Map P.EmployeeId (These P.Employee P.Employee) -- ^ "diff"
comparePersonio old new =
    Map.mapMaybe id $ alignWith f (IdMap.toMap old) (IdMap.toMap new)
  where
    f x = case x of
        These o n | o == n -> Nothing
        _                  -> Just x
