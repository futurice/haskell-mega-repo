{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Futuroom where

import Data.Aeson
import Data.Maybe
import Data.Ord
import Data.Set.Lens               (setOf)
import Futurice.Cache
import Futurice.Lucid.Foundation   (HtmlPage)
import Futurice.Office
import Futurice.Prelude
import Futurice.Servant
import Network.Google.AppsCalendar
       (eAttendees, eEnd, eStart, eStatus, eSummary, eaEmail, eaResponseStatus,
       edtDateTime)
import Network.Google.Directory
import Prelude ()
import Servant
import Servant.Server.Generic

import Futurice.App.Futuroom.API
import Futurice.App.Futuroom.Config
import Futurice.App.Futuroom.Ctx
import Futurice.App.Futuroom.IndexPage
import Futurice.App.Futuroom.Types

import qualified Data.Aeson.Lens        as L
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Map               as M
import qualified Data.Set               as S
import qualified Google                 as G
import qualified Google.Haxl            as GHaxl
import qualified Haxl.Core              as H

officeMeetingRooms :: Value
officeMeetingRooms = $(makeRelativeToProject "meeting-rooms.json" >>= embedFromJSON (Proxy :: Proxy Value))

fetchMeetingRooms :: Office -> S.Set Text
fetchMeetingRooms office = setOf (L.key (officeToText office) . L.values . L._String) officeMeetingRooms

toGoogleCfg :: Ctx -> G.GoogleCredentials
toGoogleCfg ctx = G.GoogleCredentials
       { G.privateKey = decodeUtf8Lenient $ Base64.decodeLenient $ cfgGooglePrivateKey (ctxConfig ctx)
       , G.clientId = cfgGoogleClientId (ctxConfig ctx)
       , G.clientEmail = cfgGoogleClientEmail (ctxConfig ctx)
       , G.privateKeyId = cfgGooglePrivateKeyId (ctxConfig ctx)
       , G.serviceAccountUser = cfgServiceAccountUser (ctxConfig ctx)
       }

fetchCalendarResources :: H.GenHaxl u [MeetingRoom]
fetchCalendarResources = do
    items <- GHaxl.calendarResources
    pure $ catMaybes $ toMeetingRoom <$> items
  where
      toMeetingRoom res = case (res ^. crResourceName, res ^. crResourceEmail) of
        (Just n, Just e) -> Just $ MeetingRoom n e
        _                -> Nothing

fetchEvents :: Day -> Text ->  H.GenHaxl u [Reservation]
fetchEvents reservationDay roomEmail = do
    events <- GHaxl.events reservationDay (succ reservationDay) roomEmail
    pure $ toReservation <$> filter (\e -> e ^. eStatus /= Just "cancelled" && roomReservationNotCancelled e) events
 where
     roomReservationNotCancelled e = not $ any (\a -> a ^.  eaEmail == Just roomEmail && a ^. eaResponseStatus == Just "declined" ) $ e ^. eAttendees
     toTime ev = join (ev ^? _Just . edtDateTime)
     toReservation event = Reservation
         { resStartTime = toTime (event ^. eStart)
         , resEndTime = toTime (event ^. eEnd)
         , resTitle = event ^. eSummary
         }

-- TODO: make this function
reservationsGetImpl :: Ctx -> Maybe WeekNumber -> Maybe Year -> Handler [Reservation]
reservationsGetImpl _ _ _ = do
    pure []

fetchMeetingRoomEvents :: Day -> H.GenHaxl u (Map MeetingRoom [Reservation])
fetchMeetingRoomEvents reservationDay = do
    cresources <- fetchCalendarResources
    let meetingRooms = filter isHelsinkiRoom cresources
    M.fromList <$> traverse (fetchRoomEvents reservationDay) meetingRooms
  where
      isHelsinkiRoom r = S.member (mrName r) $ fetchMeetingRooms offHelsinki
      fetchRoomEvents d r = do
          events <- fetchEvents d (mrEmail r)
          pure (r, sortBy (comparing resEndTime) events)

indexPageGetImpl :: Ctx -> Maybe Day -> Handler (HtmlPage "indexpage")
indexPageGetImpl ctx time = do
    reservationDay <- case time of
          Just t -> pure t
          Nothing -> currentDay
    let stateStore = H.stateSet (GHaxl.initDataSource (toGoogleCfg ctx) (ctxManager ctx)) H.stateEmpty
    haxlEnv <- liftIO $ H.initEnv stateStore ()
    meetingRoomEvents <- liftIO $ (H.runHaxl haxlEnv $ fetchMeetingRoomEvents reservationDay )
    pure $ indexPage reservationDay $ meetingRoomEvents

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
