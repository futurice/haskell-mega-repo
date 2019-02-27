{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Schedule where

import Control.Concurrent.STM    (atomically, readTVar, readTVarIO)
import FUM.Types.Login
import Futurice.IdMap            (Key, fromFoldable)
import Futurice.Integrations     (runIntegrations)
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant
import Servant.Server.Generic

import Futurice.App.Schedule.API
import Futurice.App.Schedule.Command
import Futurice.App.Schedule.Command.AddEventTemplates
import Futurice.App.Schedule.Command.AddScheduleTemplate
import Futurice.App.Schedule.Command.CreateSchedule
import Futurice.App.Schedule.Command.Definition
import Futurice.App.Schedule.Config
import Futurice.App.Schedule.Ctx
import Futurice.App.Schedule.Pages.CreateNewSchedulePage
import Futurice.App.Schedule.Pages.IndexPage
import Futurice.App.Schedule.Pages.NewSchedulePage
import Futurice.App.Schedule.Pages.PersonalSchedulesPage
import Futurice.App.Schedule.Pages.PersonSchedule
import Futurice.App.Schedule.Pages.SchedulingRequestPage
import Futurice.App.Schedule.SchedulePdf
import Futurice.App.Schedule.Transactor
import Futurice.App.Schedule.Types.Phase
import Futurice.App.Schedule.Types.Schedule
import Futurice.App.Schedule.Types.Templates
import Futurice.App.Schedule.Types.World

import qualified Personio as P

server :: Ctx -> Server ScheduleAPI
server ctx = genericServer $ Record
    { createScheduleTemplate   = createScheduleTemplateImpl ctx
    , addEventTemplates        = addEventTemplatesImpl ctx
    , scheduleTemplateGet      = scheduleTemplateGetImpl ctx
    , schedulePdfGet           = schedulePdfGetImpl ctx
    }

htmlServer :: Ctx -> Server HtmlAPI
htmlServer ctx = genericServer $ HtmlRecord
    { indexPageGet               = getIndexPage ctx
    , newSchedulePageGet         = newSchedulePageGetImpl ctx
    , schedulingRequestPageGet   = schedulingRequestPageGetImpl ctx
    , personalSchedulesPageGet   = personalSchedulesPageGetImpl ctx
    , createScheduleTemplateForm = createScheduleTemplateFormImpl ctx
    , createNewScheduleStartForm = createNewScheduleStartFormImpl ctx
    , createNewScheduleForm      = createNewScheduleFormImpl ctx
    , personSchedulePageGet      = personSchedulePageGetImpl ctx
    }

getIndexPage :: Ctx -> Handler (HtmlPage "indexpage")
getIndexPage ctx = do
    w <- liftIO $ readTVarIO (ctxWorld ctx)
    pure $ indexPage ctx w

newSchedulePageGetImpl :: Ctx -> Handler (HtmlPage "new-schedule-page")
newSchedulePageGetImpl ctx = do
    now <- currentTime
    emps <- liftIO $ runIntegrations mgr lgr now integrationCfg P.personioEmployees
    w <- liftIO $ readTVarIO (ctxWorld ctx)
    pure $ newSchedulePage emps w
  where
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    integrationCfg = cfgIntegrationsConfig (ctxConfig ctx)

schedulingRequestPageGetImpl :: Ctx -> Handler (HtmlPage "scheduling-request-page")
schedulingRequestPageGetImpl ctx = do
    now <- currentTime
    w <- liftIO $ readTVarIO (ctxWorld ctx)
    emps <- liftIO $ runIntegrations mgr lgr now integrationCfg P.personioEmployees
    pure $ schedulingRequestPage w emps
  where
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    integrationCfg = cfgIntegrationsConfig (ctxConfig ctx)

personalSchedulesPageGetImpl :: Ctx -> Handler (HtmlPage "personal-schedules-page")
personalSchedulesPageGetImpl ctx = do
    now <- currentTime
    w <- liftIO $ readTVarIO (ctxWorld ctx)
    emps <- liftIO $ runIntegrations mgr lgr now integrationCfg P.personioEmployees
    pure $ personalSchedulesPage w (fromFoldable emps)
  where
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    integrationCfg = cfgIntegrationsConfig (ctxConfig ctx)

processInputCommand :: (Command cmd, ICT cmd) => Ctx -> Maybe Login -> cmd 'Input -> Handler (CommandResponse ())
processInputCommand ctx loc cmd = withAuthUser ctx loc $ \login world -> runLogT "process-command" (ctxLogger ctx) $ do
    now <- currentTime
    cmdInternal' <- hoist liftIO $ runExceptT $
        runReaderT (processCommand now login cmd) world
    case cmdInternal' of
      Left err -> pure (CommandResponseError err)
      Right cmdInternal -> do
          res <- hoist liftIO $ transact ctx now login (someCommand cmdInternal)
          case res of
            Right res' -> pure res'
            Left err   -> pure (CommandResponseError err)

createScheduleTemplateImpl :: Ctx -> Maybe Login -> AddScheduleTemplate 'Input -> Handler (CommandResponse ())
createScheduleTemplateImpl = processInputCommand

createNewScheduleFormImpl :: Ctx -> Maybe Login -> CreateSchedule 'Input -> Handler (HtmlPage "indexpage")
createNewScheduleFormImpl ctx loc cmd = do
    response <- processInputCommand ctx loc cmd
    case response of
      CommandResponseOk _ -> getIndexPage ctx
      _ -> throwError err400

createScheduleTemplateFormImpl :: Ctx -> Maybe Login -> AddScheduleTemplate 'Input -> Handler (HtmlPage "indexpage")
createScheduleTemplateFormImpl ctx loc cmd = do
    response <- createScheduleTemplateImpl ctx loc cmd
    case response of
      CommandResponseOk _ -> getIndexPage ctx
      _ -> throwError err400

addEventTemplatesImpl :: Ctx -> Maybe Login -> AddEventTemplates 'Input -> Handler (CommandResponse ())
addEventTemplatesImpl = processInputCommand

scheduleTemplateGetImpl :: Ctx -> Handler [ScheduleTemplate]
scheduleTemplateGetImpl ctx =  do
    w <- liftIO $ readTVarIO (ctxWorld ctx)
    pure $ toList $ w ^. worldScheduleTemplates

createNewScheduleStartFormImpl :: Ctx -> Maybe Login -> CreateScheduleStart -> Handler (HtmlPage "create-new-schedule-page")
createNewScheduleStartFormImpl ctx _loc scheduleStart = do
    w <- liftIO $ readTVarIO (ctxWorld ctx)
    now <- currentTime
    emps <- liftIO $ runIntegrations mgr lgr now integrationCfg P.personioEmployees
    pure $ createNewSchedulePage scheduleStart emps w
  where
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    integrationCfg = cfgIntegrationsConfig (ctxConfig ctx)

schedulePdfGetImpl :: Ctx -> Key Schedule -> Handler SchedulePdf
schedulePdfGetImpl ctx sid = do
    w <- liftIO $ readTVarIO (ctxWorld ctx)
    case generateSchedulePdf w sid of
      Just s -> pure s
      Nothing -> throwError err400

personSchedulePageGetImpl :: Ctx -> P.EmployeeId -> Key Schedule -> Handler (HtmlPage "person-schedule")
personSchedulePageGetImpl ctx eid sid = do
    now <- currentTime
    w <- liftIO $ readTVarIO (ctxWorld ctx)
    emps <- liftIO $ runIntegrations mgr lgr now integrationCfg P.personioEmployees
    case (fromFoldable emps) ^. at eid of
      Just e -> pure $ personSchedule w sid e
      Nothing -> throwError err400 { errBody = "No employee found with given id"}
  where
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    integrationCfg = cfgIntegrationsConfig (ctxConfig ctx)

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
