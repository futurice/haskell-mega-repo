{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Futuroom where

import Data.Maybe
import Futurice.Cache
import Futurice.Integrations     (runIntegrations)
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant
import Servant.Server.Generic

import Futurice.App.Futuroom.API
import Futurice.App.Futuroom.Config
import Futurice.App.Futuroom.Ctx
import Futurice.App.Futuroom.IndexPage
import Futurice.App.Futuroom.Logic
import Futurice.App.Futuroom.Types

reservationsGetImpl :: Ctx -> Maybe Day -> Handler [Reservation]
reservationsGetImpl ctx day = do
    reservationDay <- case day of
          Just t -> pure t
          Nothing -> currentDay
    now <- currentTime
    meetingRoomEvents <- liftIO $ runIntegrations mgr lgr now googleCfg $ fetchMeetingRoomEvents reservationDay
    pure $ foldr (<>) [] meetingRoomEvents
  where
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    googleCfg = cfgGoogleConfig $ ctxConfig ctx

indexPageGetImpl :: Ctx -> Maybe Day -> Maybe Text -> Handler (HtmlPage "indexpage")
indexPageGetImpl ctx time mfloor = do
    reservationDay <- case time of
          Just t -> pure t
          Nothing -> currentDay
    now <- currentTime
    meetingRoomEvents <- liftIO $ runIntegrations mgr lgr now googleCfg $ fetchMeetingRoomEvents reservationDay
    pure $ indexPage reservationDay meetingRoomEvents (if mfloor == Just "show-all" then Nothing else mfloor)
  where
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    googleCfg = cfgGoogleConfig $ ctxConfig ctx

apiServer :: Ctx -> Server FuturoomAPI
apiServer ctx = genericServer $ Record
    { reservationsGet = reservationsGetImpl ctx
    }

htmlServer :: Ctx -> Server HtmlAPI
htmlServer ctx = genericServer $ HtmlRecord
    { indexPageGet = indexPageGetImpl ctx
    }

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService          .~ FuturoomService
    & serverDescription      .~ "Futuroom 1.0"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    & serverHtmlApp htmlApi  .~ htmlServer
    & serverApp futuroomApi  .~ apiServer
    & serverEnvPfx           .~ "FUTUROOMAPP"

makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
makeCtx cfg lgr mgr cache _ = do

    let ctx = Ctx cfg lgr mgr cache

    return (ctx, [])
