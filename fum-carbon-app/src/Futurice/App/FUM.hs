{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.FUM (defaultMain) where

import Control.Concurrent.STM (atomically, writeTVar)
import Futurice.Integrations  (runIntegrations)
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
import qualified Personio       as P

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
makeCtx cfg@Config {..} lgr mgr _cache mq = do
    -- employees
    let fetchEmployees = do
            now <- currentTime
            runIntegrations mgr lgr now cfgIntegrationsConfig P.personioEmployees
    employees <- fetchEmployees

    -- context
    ctx <- newCtx lgr mgr cfg
        (IdMap.fromFoldable employees)

    -- jobs
    let employeesJob = mkJob "Update personio data" (updateJob ctx fetchEmployees) $ tail $ every 300

    -- listen to MQ, especially updated Personio
    void $ forEachMessage mq $ \msg -> case msg of
        PersonioUpdated -> updateJob ctx fetchEmployees
        _               -> pure ()

    pure (ctx, [ employeesJob ])
  where
    updateJob :: Ctx -> IO [P.Employee] -> IO ()
    updateJob ctx fetchEmployees = do
        employees <- fetchEmployees
        atomically $ do
            writeTVar (ctxPersonio ctx) (IdMap.fromFoldable employees)
