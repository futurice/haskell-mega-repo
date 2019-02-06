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
import Futurice.App.Schedule.Transactor
import Futurice.App.Schedule.World

server :: Ctx -> Server ScheduleAPI
server ctx = genericServer $ Record
    { indexPageGet = getIndexPage ctx
    , createScheduleTemplate = createScheduleTemplateImpl ctx
    }

getIndexPage :: Ctx -> Handler (HtmlPage "indexpage")
getIndexPage ctx = do
    w <- liftIO $ readTVarIO (ctxWorld ctx)
    pure $ indexPage w

createScheduleTemplateImpl :: Ctx -> Maybe Login -> AddScheduleTemplate 'Input -> Handler (CommandResponse ())
createScheduleTemplateImpl ctx loc cmdInput = withAuthUser ctx loc $ \login world -> runLogT "process-command" (ctxLogger ctx) $ do
    now <- currentTime
    cmdInternal' <- hoist liftIO $ runExceptT $
        runReaderT (processCommand now login cmdInput) world
    logTrace_ "We are here"
    case cmdInternal' of
      Left err -> pure (CommandResponseError err)
      Right cmdInternal -> do
          res <- hoist liftIO $ transact ctx now login (someCommand cmdInternal)
          case res of
            Right res' -> pure res'
            Left err   -> pure (CommandResponseError err)


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

withAuthUser :: Ctx -> Maybe Login -> (Login -> World -> Handler a) -> Handler a
withAuthUser ctx loc f = case loc <|> cfgMockUser cfg of
    Nothing    -> throwError err403
    Just login -> do
        world <- liftIO $ atomically $ readTVar (ctxWorld ctx)
        f login world
  where
    cfg = ctxConfig ctx
