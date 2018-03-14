{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.PersonioProxy (defaultMain) where

import Control.Concurrent.STM (atomically, newTVarIO, writeTVar)
import Futurice.IdMap         (IdMap)
import Futurice.Prelude
import Prelude ()

import Futurice.Periocron
import Futurice.Servant
import Servant

import Futurice.App.PersonioProxy.API
import Futurice.App.PersonioProxy.Config
import Futurice.App.PersonioProxy.Logic
import Futurice.App.PersonioProxy.Types

import qualified Futurice.IdMap as IdMap
import qualified Personio

server :: Ctx -> Server PersonioProxyAPI
server ctx = pure "Try /swagger-ui/"
    :<|> personioRequest ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverName          .~ "Personio Proxy"
    & serverDescription   .~ "Make faster queries to Personio"
    & serverColour        .~ (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    & serverApp personioProxyApi .~ server
    & serverEnvPfx        .~ "PERSONIOPROXY"

newCtx
    :: Logger
    -> IdMap Personio.Employee
    -> [Personio.EmployeeValidation]
    -> IO Ctx
newCtx lgr es vs = do
    Ctx lgr <$> newTVarIO es <*> newTVarIO vs

makeCtx :: Config -> Logger -> Manager -> Cache -> IO (Ctx, [Job])
makeCtx (Config cfg) lgr mgr _cache = do
    -- employees
    let fetchEmployees = Personio.evalPersonioReqIO mgr lgr cfg Personio.PersonioAll
    (employees, validations) <- fetchEmployees

    -- context
    ctx <- newCtx lgr (IdMap.fromFoldable employees) validations

    -- jobs
    let employeesJob = mkJob "Update personio data" (updateJob ctx fetchEmployees) $ tail $ every 300

    pure (ctx, [ employeesJob ])
  where
    updateJob :: Ctx -> IO ([Personio.Employee], [Personio.EmployeeValidation]) -> IO ()
    updateJob ctx fetchEmployees = do
        (employees, validations) <- fetchEmployees
        atomically $ do
            writeTVar (ctxPersonio ctx) (IdMap.fromFoldable employees)
            writeTVar (ctxPersonioValidations ctx) validations
