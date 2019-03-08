{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Futuroom.Logic where

import Data.Ord
import Data.Set.Lens               (setOf)
import Futurice.App.Futuroom.Types
import Futurice.Integrations       (Integrations, ServGO)
import Futurice.Office
import Futurice.Prelude
import Network.Google.AppsCalendar
       (eAttendees, eEnd, eStart, eStatus, eSummary, eaEmail, eaResponseStatus,
       edtDateTime)
import Network.Google.Directory
import Prelude ()

import qualified Data.Aeson.Lens as L
import qualified Data.Map        as M
import qualified Data.Set        as S
import qualified Google          as G

officeMeetingRooms :: Value
officeMeetingRooms = $(makeRelativeToProject "meeting-rooms.json" >>= embedFromJSON (Proxy :: Proxy Value))

fetchMeetingRooms :: Office -> S.Set Text
fetchMeetingRooms office = setOf (L.key (officeToText office) . L.values . L._String) officeMeetingRooms

-----------------------------------------------------------------------------
-- Integrations
-----------------------------------------------------------------------------

fetchEvents :: Day -> Text -> Integrations '[ServGO] [Reservation]
fetchEvents reservationDay roomEmail = do
    events <- G.googleCalendarEvents G.ReadOnly reservationDay (succ reservationDay) roomEmail
    pure $ toReservation <$> filter (\e -> e ^. eStatus /= Just "cancelled" && roomReservationNotCancelled e) events
 where
     roomReservationNotCancelled e = not $ any (\a -> a ^.  eaEmail == Just roomEmail && a ^. eaResponseStatus == Just "declined" ) $ e ^. eAttendees
     toTime ev = join (ev ^? _Just . edtDateTime)
     toReservation event = Reservation
         { resStartTime = toTime (event ^. eStart)
         , resEndTime = toTime (event ^. eEnd)
         , resTitle = event ^. eSummary
         }

fetchCalendarResources :: Integrations '[ServGO] [MeetingRoom]
fetchCalendarResources = do
    items <- G.googleCalendarResources G.ReadOnly
    pure $ catMaybes $ toMeetingRoom <$> items
  where
      toMeetingRoom res = case (res ^. crResourceName, res ^. crResourceEmail) of
        (Just n, Just e) -> Just $ MeetingRoom
          { mrName = n
          , mrEmail = e
          , mrLocation = res ^. crBuildingId
          , mrCapasity = fromIntegral <$> res ^. crCapacity
          , mrFloor = res ^. crFloorName
          }
        _                -> Nothing

fetchMeetingRoomEvents :: Day -> Integrations '[ServGO] (Map MeetingRoom [Reservation])
fetchMeetingRoomEvents reservationDay = do
    cresources <- fetchCalendarResources
    let meetingRooms = filter isHelsinkiRoom cresources
    M.fromList <$> traverse (fetchRoomEvents reservationDay) meetingRooms
  where
      isHelsinkiRoom r = S.member (mrName r) $ fetchMeetingRooms offHelsinki
      fetchRoomEvents d r = do
          events <- fetchEvents d (mrEmail r)
          pure (r, sortBy (comparing resEndTime) events)
