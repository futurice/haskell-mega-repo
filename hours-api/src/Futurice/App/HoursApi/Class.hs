{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
module Futurice.App.HoursApi.Class (
    -- * Monad class
    MonadHours (..),
    -- * Data structures
    -- | Distinct data types are used, to decouple from PlanMill.

    -- ** Project
    Project (..),
    projectId,
    projectName,
    projectClosed,
    projectAbsence,
    -- ** Tasks
    Task (..),
    taskId,
    taskName,
    taskProjectId,
    taskFinish,
    -- ** Timereports
    Timereport (..),
    timereportId,
    timereportTaskId,
    timereportProjectId,
    timereportDay,
    timereportComment,
    timereportAmount,
    timereportType,
    timereportClosed,
    -- ** New timereport
    NewTimereport (..),
    newTimereportTaskId,
    newTimereportDay,
    newTimereportAmount,
    newTimereportComment,
    -- ** Capacity
    Capacity (..),
    capacityDay,
    capacityAmount,
    capacityDescription,
    -- ** Reportable assignment
    ReportableAssignment (..),
    raFinish,
    raProjectId,
    raTaskId,
    ) where

import Data.Fixed                (Centi)
import Data.Time                 (addDays)
import FUM                       (Login)
import Futurice.Prelude
import Futurice.Time
import Numeric.Interval.NonEmpty (Interval, (...))
import Prelude ()

import qualified Futurice.App.HoursApi.Types as T
import qualified PlanMill                    as PM

-- | The core interface, in which "Futurice.App.HoursApi.Logic" is written.
--
-- We don't reuse "PlanMill" records, to make mocking simpler
-- (i.e. we omit unrelevant fields).
--
-- /TODO:/ add 'MonadLog' pre-req
--
class (MonadTime m) => MonadHours m where
    -- Volatile things, we probably ask from PlanMill

    -- | Profile picture url.
    profilePictureUrl :: m Text

    -- | Profile first name.
    profileFirstName :: m Text

    -- | Profile last name.
    profileLastName :: m Text

    -- | My remaining vacations.
    vacationRemaining :: m (NDT 'Days Centi)

    -- | My flex balance.
    flexBalance :: m (NDT 'Hours Centi)

    -- | My orking hours per day.
    workingHours :: m (NDT 'Hours Centi)

    -- | Project task.
    task :: PM.TaskId -> m Task

    -- | Project.
    project :: PM.ProjectId -> m Project

    -- | Projects I can report ATM.
    reportableAssignments :: m [ReportableAssignment]

    -- | Timereports for the interval (inclusive).
    timereports :: Interval Day -> m [Timereport]

    -- | Single timereport.
    --
    -- /TODO:/ check only "my" timereports are returned.
    timereport :: PM.TimereportId -> m Timereport

    -- | Delete timereport
    --
    -- /TODO:/ check only "my" timereports are deleted
    deleteTimereport :: PM.TimereportId -> m ()

    -- | New timereport
    addTimereport :: NewTimereport -> m ()

    -- | Edit timereport
    --
    -- /TODO:/ check only "my" timereports are deleted
    editTimereport :: PM.TimereportId -> NewTimereport -> m ()

    -- | Capacities.
    --
    -- Returns a capacity for each day in the interval.
    capacities :: Interval Day -> m [Capacity]

    -- Not-so-volatile things!

    -- | My Latest entry.
    --
    -- "Best" guess for the entry for the project.
    --
    -- /Note:/ we reuse 'LatestEntry' as a result type.
    latestEntry :: PM.TaskId -> m (Maybe T.LatestEntry)
    latestEntry _ = return Nothing

    -- | Return timereports from last 28 days
    --
    -- Concrete implementations are free to provide own variants
    -- (e.g. use more aggressive caching!)
    timereportsLast28 :: m [Timereport]
    timereportsLast28 = do
        today <- currentDay
        timereports (addDays (-28) today ... today)

    -- | Settings
    --
    -- Return user preferences
    settings :: m T.SettingsResponse

    -- | Edit settings
    --
    -- Edit user settings
    editSettings :: T.SettingsResponse -> T.SettingsResponse -> m Bool

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data ReportableAssignment = ReportableAssignment
    { _raProjectId :: !PM.ProjectId
    , _raTaskId    :: !PM.TaskId
    , _raFinish    :: !UTCTime
    }
  deriving (Eq, Show, Generic)

data Task = Task
    { _taskId        :: !PM.TaskId
    , _taskName      :: !Text
    , _taskProjectId :: !PM.ProjectId
    , _taskFinish    :: !UTCTime
    }
  deriving (Eq, Show, Generic)

data Project = Project
    { _projectId      :: !PM.ProjectId
    , _projectName    :: !Text
    , _projectClosed  :: !Bool
    , _projectAbsence :: !Bool
    }
  deriving (Eq, Show, Generic)

data Timereport = Timereport
    { _timereportId        :: !PM.TimereportId
    , _timereportTaskId    :: !PM.TaskId
    , _timereportProjectId :: !PM.ProjectId
    , _timereportDay       :: !Day
    , _timereportComment   :: !Text
    , _timereportAmount    :: !(NDT 'Hours Centi)
    , _timereportType      :: !T.EntryType
    , _timereportClosed    :: !Bool -- ^ i.e. not editable
    }
  deriving (Eq, Show, Generic)

data NewTimereport = NewTimereport
    { _newTimereportTaskId  :: !PM.TaskId
    , _newTimereportDay     :: !Day
    , _newTimereportAmount  :: !(NDT 'Hours Centi)
    , _newTimereportComment :: !Text
    }
  deriving (Eq, Show, Generic)

data Capacity = Capacity
    { _capacityDay          :: !Day
    , _capacityAmount       :: !(NDT 'Hours Centi)
    , _capacityDescription  :: !(Maybe Text)  -- ^ name of the day
    }
  deriving (Eq, Show, Generic)

-------------------------------------------------------------------------------
-- Lenses
-------------------------------------------------------------------------------

makeLenses ''Capacity
makeLenses ''NewTimereport
makeLenses ''Project
makeLenses ''ReportableAssignment
makeLenses ''Task
makeLenses ''Timereport
