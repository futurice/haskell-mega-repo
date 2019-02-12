{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Futurice.App.Schedule.World where

import Data.Time.LocalTime
import Futurice.IdMap      (HasKey, IdMap, Key, key)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Schedule.Types

import qualified Personio as P

data Starter = Starter
    { _starterName       :: !Text
    , _starterEmail      :: !Text
    , _starterSupervisor :: !(Maybe P.Employee)
    , _starterPersonioID :: !(Maybe P.EmployeeId)
    } deriving (Show)

data MeetingRoom = MeetingRoom

data Event = Event

data EventTask = EventTask

newtype MonthOffset = MonthOffset Integer

newtype DayOffset = DayOffset Integer

--   summary     |    description    | dayOffset | startTime | endTime  | inviteEmployees | inviteSupervisors | isCollective | scheduleTemplate_id | monthOffset
data EventTemplate = EventTemplate
    { _etSummary         :: !Text
    , _etDescription     :: !Text
    , _etTimeOffset      :: !(Either DayOffset MonthOffset)
    , _etStartTime       :: !(NominalDiffTime) -- TODO: Duration?3
    , _etEndTime         :: !(NominalDiffTime)
    , _etInviteEmployees :: !(Bool)
    }

newtype Identifier a = Identifier UUID
    deriving (Show, Eq, Ord)

data World = World
    { _worldStarters           :: ![Starter]
    , _worldScheduleTemplates  :: !(IdMap ScheduleTemplate)
    , _worldSchedulingRequests :: !(IdMap SchedulingRequest)
    } deriving Show

makeLenses ''World

emptyWorld :: World
emptyWorld = World [] mempty mempty

 -- id            | integer                  |           | not null | nextval('futuschedule_futuuser_id_seq'::regclass)
 -- password      | character varying(128)   |           | not null |
 -- last_login    | timestamp with time zone |           |          |
 -- username      | character varying(40)    |           | not null |
 -- email         | character varying(100)   |           | not null |
 -- first_name    | character varying(100)   |           | not null |
 -- last_name     | character varying(100)   |           | not null |
 -- is_active     | boolean                  |           | not null |
 -- is_admin      | boolean                  |           | not null |
 -- supervisor_id | integer                  |           |          |
 -- name          | character varying(255)   |           |          |
 -- personio_id   | integer                  |           |          |
