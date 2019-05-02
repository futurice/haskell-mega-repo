{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Schedule.Types.Schedule where

import Data.Swagger      (NamedSchema (..), ToSchema (..))
import Futurice.Generics
import Futurice.Prelude
import Futurice.IdMap (Key, HasKey, key)
import Prelude ()
import FUM.Types.Login
import Futurice.Lucid.Foundation

import qualified Personio as P
import qualified Data.Set as S

data EventRequest = EventRequest
    { _eventRequestSummary      :: !Text
    , _eventRequestDescription  :: !Text
    , _eventRequestLocations    :: ![Text]   -- TODO: change to location type
    , _eventRequestStartTime    :: !TimeOfDay
    , _eventRequestEndTime      :: !TimeOfDay
    , _eventRequestEmployees    :: ![P.EmployeeId]
    , _eventRequestIsCollective :: !Bool
    } deriving (Typeable, GhcGeneric, SopGeneric, HasDatatypeInfo)
      deriving (ToJSON) via (Sopica EventRequest)
      deriving (FromJSON) via (Sopica EventRequest)

makeLenses ''EventRequest

type ScheduleRequest = [EventRequest]

instance ToSchema EventRequest where
    declareNamedSchema _ = pure $ NamedSchema (Just "EventRequest") mempty

data Event  = Event
    { _eventSummary              :: !Text
    , _eventDescription          :: !Text
    , _eventLocations            :: ![Text]   -- TODO: change to location type
    , _eventStartTime            :: !TimeOfDay
    , _eventEndTime              :: !TimeOfDay
    , _eventInviteEmployees      :: !Bool
    , _eventInviteSupervisors    :: !Bool
    , _eventIsCollective         :: !Bool
    , _eventEmployees            :: !(S.Set P.EmployeeId)
    } deriving (Typeable, GhcGeneric, SopGeneric, HasDatatypeInfo)
      deriving (ToJSON) via (Sopica Event)
      deriving (FromJSON) via (Sopica Event)

makeLenses ''Event

instance ToSchema Event where
    declareNamedSchema _ = pure $ NamedSchema (Just "Event") mempty

data ScheduleStatus = ScheduleStatusInProgress
                    | ScheduleStatusSuccess
                    | ScheduleStatusError
                    | ScheduleStatusActionFailed

instance Show ScheduleStatus where
    show ScheduleStatusInProgress   = "In Progress"
    show ScheduleStatusSuccess      = "Success"
    show ScheduleStatusError        = "Error"
    show ScheduleStatusActionFailed = "Action Failed"

instance ToHtml ScheduleStatus where
    toHtmlRaw = toHtml
    toHtml s@ScheduleStatusSuccess      = span_ [ class_ "label success"] $ toHtml $ textShow s
    toHtml s@ScheduleStatusInProgress   = span_ [ class_ "label warning"] $ toHtml $ textShow s
    toHtml s@ScheduleStatusError        = span_ [ class_ "label alert"] $ toHtml $ textShow s
    toHtml s@ScheduleStatusActionFailed = span_ [ class_ "label alert"] $ toHtml $ textShow s

data Schedule = Schedule
    { _scheduleTemplateName     :: !Text
    , _scheduleEvents           :: ![Event]
    , _scheduleCreatedBy        :: !Login
    , _scheduleCreatedOn        :: !UTCTime
    , _scheduleEventIds         :: ![Maybe Text]
    }

makeLenses ''Schedule

instance HasKey Schedule where
    type Key Schedule = UTCTime
    key = scheduleCreatedOn -- TODO: change to something better

peopleOnSchedule :: Schedule -> S.Set P.EmployeeId
peopleOnSchedule schedule = fold $ _eventEmployees <$> schedule ^. scheduleEvents

scheduleStatus :: Schedule -> ScheduleStatus
scheduleStatus _ = ScheduleStatusSuccess --TODO: check real status
