{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.FUM (defaultMain) where

import Control.Concurrent.STM (atomically, writeTVar)
import Futurice.Periocron
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant

import Futurice.App.FUM.API
import Futurice.App.FUM.Command.Server       (commandServer)
import Futurice.App.FUM.Config
import Futurice.App.FUM.Ctx
import Futurice.App.FUM.Machine
import Futurice.App.FUM.Markup
import Futurice.App.FUM.Pages.Server
import Futurice.App.FUM.Report.CompareOldFum

import qualified Futurice.IdMap as IdMap
import qualified Personio

-------------------------------------------------------------------------------
-- Server
-------------------------------------------------------------------------------

server :: Ctx -> Server FumCarbonApi
server ctx = pagesServer ctx
    :<|> compareOldFumReportImpl ctx
    :<|> commandServer ctx
    :<|> machineServer ctx

-------------------------------------------------------------------------------
-- Reports
-------------------------------------------------------------------------------

compareOldFumReportImpl :: Ctx -> Handler (HtmlPage "compare-old-fum-report")
compareOldFumReportImpl = liftIO . compareOldFumReport

-------------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverName             .~ "FUM Carbon"
    & serverDescription      .~ "FUM faster than ever"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF2 'AC2))
    & serverApp fumCarbonApi .~ server
    & serverEnvPfx           .~ "FUMAPP"

makeCtx :: Config -> Logger -> Manager -> Cache -> IO (Ctx, [Job])
makeCtx cfg@Config {..} lgr mgr _cache = do
    -- employees
    let fetchEmployees = Personio.evalPersonioReqIO mgr lgr cfgPersonioCfg Personio.PersonioAll
    (employees, validations) <- fetchEmployees

    -- context
    ctx <- newCtx lgr mgr cfg
        (IdMap.fromFoldable employees)
        validations

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
