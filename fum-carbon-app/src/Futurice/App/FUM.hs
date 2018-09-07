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
    & serverService          .~ FumCarbonService
    & serverDescription      .~ "FUM faster than ever"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF2 'AC2))
    & serverApp fumCarbonApi .~ server
    & serverEnvPfx           .~ "FUMAPP"

makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
makeCtx cfg@Config {..} lgr mgr _cache _mq = do
    -- employees
    let fetchData = Personio.evalPersonioReqIO mgr lgr cfgPersonioCfg Personio.PersonioAll
    allData <- fetchData
    let employees = Personio.paEmployees allData

    -- context
    ctx <- newCtx lgr mgr cfg
        (IdMap.fromFoldable employees)
        allData

    -- jobs
    let employeesJob = mkJob "Update personio data" (updateJob ctx fetchData) $ tail $ every 300

    pure (ctx, [ employeesJob ])
  where
    updateJob :: Ctx -> IO Personio.PersonioAllData -> IO ()
    updateJob ctx fetchData = do
        allData <- fetchData
        let employees = Personio.paEmployees allData
        atomically $ do
            writeTVar (ctxPersonio ctx) (IdMap.fromFoldable employees)
            writeTVar (ctxPersonioData ctx) allData
