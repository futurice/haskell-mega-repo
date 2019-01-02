{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Futuroom where

import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Ord
import Data.Set.Lens               (setOf)
import Futurice.Cache
import Futurice.Lucid.Foundation   (HtmlPage)
import Futurice.Office
import Futurice.Prelude
import Futurice.Servant
import Network.Google
       (envScopes, newEnvWith, newLogger, runGoogle, runResourceT, send)
import Network.Google.AppsCalendar
       (calendarReadOnlyScope, eAttendees, eEnd, eStart, eStatus, eSummary,
       eaEmail, eaResponseStatus, edtDateTime, elSingleEvents, elTimeMax,
       elTimeMin, eveItems, eventsList)
import Network.Google.Auth
import Network.Google.Directory
import Prelude ()
import Servant
import Servant.Server.Generic
import System.IO                   (stdout)

import Futurice.App.Futuroom.API
import Futurice.App.Futuroom.Config
import Futurice.App.Futuroom.Ctx
import Futurice.App.Futuroom.IndexPage
import Futurice.App.Futuroom.Types

import qualified Data.Aeson.Lens        as L
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Map               as M
import qualified Data.Set               as S
import qualified Network.Google         as G

officeMeetingRooms :: Value
officeMeetingRooms = $(makeRelativeToProject "meeting-rooms.json" >>= embedFromJSON (Proxy :: Proxy Value))

fetchMeetingRooms :: Office -> S.Set Text
fetchMeetingRooms office = setOf (L.key (officeToText office) . L.values . L._String) officeMeetingRooms

fetchCredentials :: Ctx -> Credentials s
fetchCredentials ctx = case FromAccount <$> parseEither parseJSON (toJSON $ serviceAccount) of
  Left _ -> error "Couln't read config"
  Right c -> c
 where
   serviceAccount = ServiceAccount
       { credPrivateKey = decodeUtf8Lenient $ Base64.decodeLenient $ cfgGooglePrivateKey (ctxConfig ctx) -- TODO: fix this hack
       , credClientId = cfgGoogleClientId (ctxConfig ctx)
       , credClientEmail = cfgGoogleClientEmail (ctxConfig ctx)
       , credPrivateKeyId = cfgGooglePrivateKeyId (ctxConfig ctx)
       }

fetchCalendarResources :: Ctx -> IO [MeetingRoom]
fetchCalendarResources ctx = do
    mgr <- newManager tlsManagerSettings
    let cred = serviceAccountUser serviceAccount $ fetchCredentials ctx
    lgr <- newLogger G.Error stdout
    env <- newEnvWith cred lgr mgr <&> (envScopes .~ adminDirectoryResourceCalendarReadOnlyScope)
    runResourceT $ runGoogle env $ do
        x <- send $ resourcesCalendarsList "my_customer"
        pure $ catMaybes $ toMeetingRoom <$> x ^. crsItems
  where
      serviceAccount = Just . cfgServiceAccountUser $ ctxConfig ctx
      toMeetingRoom res = case (res ^. crResourceName, res ^. crResourceEmail) of
        (Just n, Just e) -> Just $ MeetingRoom n e
        _                -> Nothing

fetchEvents :: Ctx -> Day -> Text -> IO [Reservation]
fetchEvents ctx reservationDay roomEmail = do
    mgr <- newManager tlsManagerSettings
    let cred = serviceAccountUser serviceAccount $ fetchCredentials ctx
    lgr <- newLogger G.Error stdout
    env <- newEnvWith cred lgr mgr <&> (envScopes .~ calendarReadOnlyScope)
    events <- runResourceT $ runGoogle env $ do
        eventList <- send (eventsList roomEmail
                           & elTimeMin .~ Just (UTCTime reservationDay 0)
                           & elTimeMax .~ Just (UTCTime (succ reservationDay) 0)
                           & elSingleEvents .~ Just True)
        pure $ eventList ^. eveItems
    pure $ toReservation <$> filter (\e -> e ^. eStatus /= Just "cancelled" && roomReservationNotCancelled e) events
 where
     roomReservationNotCancelled e = not $ any (\a -> a ^.  eaEmail == Just roomEmail && a ^. eaResponseStatus == Just "declined" ) $ e ^. eAttendees
     serviceAccount = Just . cfgServiceAccountUser $ ctxConfig ctx
     toTime ev = join (ev ^? _Just . edtDateTime)
     toReservation event = Reservation
         { resStartTime = toTime (event ^. eStart)
         , resEndTime = toTime (event ^. eEnd)
         , resTitle = event ^. eSummary
         }

reservationsGetImpl :: Ctx -> Maybe WeekNumber -> Maybe Year -> Handler [Reservation]
reservationsGetImpl _ _ _ = do
    pure []

indexPageGetImpl :: Ctx -> Maybe Day -> Handler (HtmlPage "indexpage")
indexPageGetImpl ctx time = do
    reservationDay <- case time of
          Just t -> pure t
          Nothing -> currentDay
    eventMap <- liftIO $ do
        rooms <- fetchCalendarResources ctx
        M.fromList <$> traverse (fetchRoomEvents reservationDay) (filter isHelsinkiRoom rooms)
    pure $ indexPage reservationDay $ eventMap
  where
      isHelsinkiRoom r = S.member (mrName r) $ fetchMeetingRooms offHelsinki
      fetchRoomEvents d r = do
          events <- fetchEvents ctx d (mrEmail r)
          pure (r, sortBy (comparing resEndTime) events)

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
