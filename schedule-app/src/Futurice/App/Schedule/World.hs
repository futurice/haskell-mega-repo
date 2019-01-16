module Futurice.App.Schedule.World where

import Data.Time.LocalTime
import Futurice.Prelude
import Prelude ()

import qualified Personio as P

data Starter = Starter
    { _starterName       :: !Text
    , _starterEmail      :: !Text
    , _starterSupervisor :: !(Maybe P.Employee)
    , _starterPersonioID :: !(Maybe P.EmployeeId)
    } deriving (Show)

data Calendar = Calendar deriving Show

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

data ScheduleTemplate = ScheduleTemplate
    { _scheduleName     :: !Text
    , _scheduleCalendar :: !Calendar
    , _scheduleTimezone :: !TimeZone
    } deriving Show

data SchedulingRequestStatus = Accepted
                             | Declined -- TODO: check what are the actual statuses
                             deriving Show

data SchedulingRequest = SchedulingRequest
    { _srJson        :: !Text -- TODO: separate to it's own type
    , _srRequestedAt :: !UTCTime -- should this be localtime?
    , _srStatus      :: !SchedulingRequestStatus
    , _srError       :: !Text -- this should not probably be it's own field?
    , _srRequestedBy :: !P.EmployeeId
    , _srPdfUrl      :: !Text  -- what was this
    } deriving Show

data World = World
    { _worldStarters          :: ![Starter]
    , _worldScheduleTemplates :: ![ScheduleTemplate]
    } deriving Show

emptyWorld :: World
emptyWorld = World [] []

data Phase = Command | Internal

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
