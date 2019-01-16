{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Schedule where

import Control.Concurrent.STM    (readTVarIO)
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant
import Servant.Server.Generic

import Futurice.App.Schedule.API
import Futurice.App.Schedule.Config
import Futurice.App.Schedule.Ctx
import Futurice.App.Schedule.Pages.IndexPage

server :: Ctx -> Server ScheduleAPI
server ctx = genericServer $ Record
    { indexPageGet = getIndexPage ctx
    }

getIndexPage :: Ctx -> Handler (HtmlPage "indexpage")
getIndexPage ctx = do
    w <- liftIO $ readTVarIO (ctxWorld ctx)
    pure $ indexPage w

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService          .~ ScheduleService
    & serverDescription      .~ "Schedule onboarding events"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF2 'AC2))
    & serverApp scheduleApi  .~ server
    & serverEnvPfx           .~ "SCHEDULEAPP"

makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
makeCtx cfg lgr mgr _cache _mq = do
    ctx <- newCtx lgr mgr cfg
    pure (ctx, [])
