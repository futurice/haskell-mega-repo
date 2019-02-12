{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Schedule where

import Control.Concurrent.STM    (atomically, readTVar, readTVarIO)
import FUM.Types.Login
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant
import Servant.Server.Generic

import Futurice.App.Schedule.API
import Futurice.App.Schedule.Command
import Futurice.App.Schedule.Command.AddScheduleTemplate
import Futurice.App.Schedule.Command.Definition
import Futurice.App.Schedule.Config
import Futurice.App.Schedule.Ctx
import Futurice.App.Schedule.Pages.IndexPage
import Futurice.App.Schedule.Pages.NewSchedulePage
import Futurice.App.Schedule.Pages.PersonalSchedulesPage
import Futurice.App.Schedule.Pages.SchedulingRequestPage
import Futurice.App.Schedule.Transactor
import Futurice.App.Schedule.World

server :: Ctx -> Server ScheduleAPI
server ctx = genericServer $ Record
    { createScheduleTemplate   = createScheduleTemplateImpl ctx
    }

htmlServer :: Ctx -> Server HtmlAPI
htmlServer ctx = genericServer $ HtmlRecord
    { indexPageGet               = getIndexPage ctx
    , newSchedulePageGet         = newSchedulePageGetImpl ctx
    , schedulingRequestPageGet   = schedulingRequestPageGetImpl ctx
    , personalSchedulesPageGet   = personalSchedulesPageGetImpl ctx
    , createScheduleTemplateForm = createScheduleTemplateFormImpl ctx
    }

getIndexPage :: Ctx -> Handler (HtmlPage "indexpage")
getIndexPage ctx = do
    w <- liftIO $ readTVarIO (ctxWorld ctx)
    pure $ indexPage ctx w

newSchedulePageGetImpl :: Ctx -> Handler (HtmlPage "new-schedule-page")
newSchedulePageGetImpl ctx = do
    w <- liftIO $ readTVarIO (ctxWorld ctx)
    pure $ newSchedulePage w

schedulingRequestPageGetImpl :: Ctx -> Handler (HtmlPage "scheduling-request-page")
schedulingRequestPageGetImpl ctx = do
    w <- liftIO $ readTVarIO (ctxWorld ctx)
    pure $ schedulingRequestPage w

personalSchedulesPageGetImpl :: Ctx -> Handler (HtmlPage "personal-schedules-page")
personalSchedulesPageGetImpl ctx = do
    w <- liftIO $ readTVarIO (ctxWorld ctx)
    pure $ personalSchedulesPage w

createScheduleTemplateImpl :: Ctx -> Maybe Login -> AddScheduleTemplate 'Input -> Handler (CommandResponse ())
createScheduleTemplateImpl ctx loc cmdInput = withAuthUser ctx loc $ \login world -> runLogT "process-command" (ctxLogger ctx) $ do
    now <- currentTime
    cmdInternal' <- hoist liftIO $ runExceptT $
        runReaderT (processCommand now login cmdInput) world
    case cmdInternal' of
      Left err -> pure (CommandResponseError err)
      Right cmdInternal -> do
          res <- hoist liftIO $ transact ctx now login (someCommand cmdInternal)
          case res of
            Right res' -> pure res'
            Left err   -> pure (CommandResponseError err)

createScheduleTemplateFormImpl :: Ctx -> Maybe Login -> AddScheduleTemplate 'Input -> Handler (HtmlPage "indexpage")
createScheduleTemplateFormImpl ctx loc cmd = do
    response <- createScheduleTemplateImpl ctx loc cmd
    case response of
      CommandResponseOk _ -> getIndexPage ctx
      _ -> throwError err400


defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService          .~ ScheduleService
    & serverDescription      .~ "Schedule onboarding events"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF2 'AC2))
    & serverHtmlApp htmlApi  .~ htmlServer
    & serverApp scheduleApi  .~ server
    & serverEnvPfx           .~ "SCHEDULEAPP"

makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
makeCtx cfg lgr mgr _cache _mq = do
    ctx <- newCtx lgr mgr cfg
    pure (ctx, [])

withAuthUser :: Ctx -> Maybe Login -> (Login -> World -> Handler a) -> Handler a
withAuthUser ctx loc f = case loc <|> cfgMockUser cfg of
    Nothing    -> throwError err403
    Just login -> do
        world <- liftIO $ atomically $ readTVar (ctxWorld ctx)
        f login world
  where
    cfg = ctxConfig ctx
