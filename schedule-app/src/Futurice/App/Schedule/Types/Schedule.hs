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
import Futurice.IdMap (Key)
import Prelude ()
import FUM.Types.Login

import Futurice.App.Schedule.Types.Templates

import qualified Personio as P
import qualified Data.Set as S

data EventRequest  = EventRequest
    { _eventRequestSummary     :: !Text
    , _eventRequestDescription :: !Text
    , _eventRequestLocations   :: ![Text]   -- TODO: change to location type
    , _eventRequestStartTime   :: !TimeOfDay
    , _eventRequestEndTime     :: !TimeOfDay
    , _eventRequestEmployees   :: ![P.EmployeeId]
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
    , _eventEmployees            :: ![P.EmployeeId]
    } deriving (Typeable, GhcGeneric, SopGeneric, HasDatatypeInfo)
      deriving (ToJSON) via (Sopica Event)
      deriving (FromJSON) via (Sopica Event)

makeLenses ''Event

instance ToSchema Event where
    declareNamedSchema _ = pure $ NamedSchema (Just "Event") mempty

data Schedule = Schedule
    { _scheduleScheduleTemplate :: !(Key ScheduleTemplate)
    , _scheduleEvents           :: ![Event]
    , _scheduleCreatedBy        :: !Login
    , _scheduleCreatedOn        :: !UTCTime
    }

makeLenses ''Schedule

peopleOnSchedule :: Schedule -> S.Set P.EmployeeId
peopleOnSchedule schedule = S.fromList $ concat $ _eventEmployees <$> schedule ^. scheduleEvents
