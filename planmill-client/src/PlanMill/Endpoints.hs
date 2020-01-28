{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Endpoints (
    -- * Absences
    absences,
    absencesFromInterval,
    absencesFromIntervalFor,
    absence,
    -- * Accounts
    accounts,
    account,
    -- * Actions
    actions,
    -- * Assignments
    projectAssignments,
    reportableAssignments,
    -- * Capacity calendars
    capacitycalendars,
    userCapacity,
    -- * Contacts
    contacts,
    -- * Enumerations
    enumerations,
    -- * Exit criteria
    exitcriteria,
    -- * Me
    me,
    -- * Projects
    project,
    projectMembers,
    projects,
    projectsWithType,
    portfolios,
    -- * Tasks
    task,
    projectTasks,
    -- * Teams
    team,
    teams,
    -- * Timereports
    timereport,
    timereports,
    timereportsFor,
    timereportsFromInterval,
    timereportsFromIntervalFor,
    -- ** Editing
    addTimereport,
    editTimereport,
    deleteTimereport,
    -- * Users
    user,
    users,
    userTimeBalance,
    userVacations,
    -- ** Editing
    editUser,
    -- * Hooks
    addHook,
    deleteHook,
    hooks,
    -- * Reports
    reports,
    allRevenuesReport,
    valueCreationByMonthReport
    ) where

import PlanMill.Internal.Prelude

import GHC.TypeLits (KnownSymbol, symbolVal)

import PlanMill.Types

import qualified Data.Map  as Map
import qualified Data.Text as T

-- | Get a list of absences.
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#absences_get>
absences :: PlanMill Absences
absences = planMillGet $ t "absences"

-- | Get a list of absences from specified time interval.
absencesFromInterval :: ResultInterval -> PlanMill Absences
absencesFromInterval ri =
    planMillPagedGetQs qs $ t "absences"
  where
    qs = intervalToQueryString ri

-- | Get a list of absences from specified time interval.
--
-- /Note:/ This is uncodumented usage but seems to work.
absencesFromIntervalFor :: ResultInterval -> UserId -> PlanMill Absences
absencesFromIntervalFor ri (Ident uid) =
    planMillPagedGetQs (qs <> qs') $ t "absences"
  where
    qs = intervalToQueryString ri
    qs' = Map.fromList
        [ ("person", fromString $ show uid)
        ]

-- | View details of single absence
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#absences__id__get>
absence :: AbsenceId -> PlanMill Absence
absence i = planMillGet $ t "absenses" // i

-- | Get a list of accounts
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#accounts_get>
accounts :: PlanMill Accounts
accounts = planMillGet $ t "accounts"

-- | A single account in PlanMill.
--
-- See <http://developers.planmill.com/api/#accounts__account_id__get>
account :: AccountId -> PlanMill Account
account aid = planMillGet $ t "accounts" // aid

-- | View details of single me.
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#me_get>
me :: PlanMill Me
me = planMillGet $ t "me"

-- | Get a list of projects
--
-- See <vttps://online.planmill.com/pmtrial/schemas/v1_5/index.html#projects_get>
projects :: PlanMill Projects
projects = planMillPagedGet $ t "projects"

-- | Get a list of projects with certain viewtemplate
--
-- See <vttps://online.planmill.com/pmtrial/schemas/v1_5/index.html#projects_get>
projectsWithType :: ViewTemplate -> PlanMill Projects
projectsWithType vt = planMillPagedGetQs qs' $ t "projects"
  where
    qs' :: QueryString
    qs' = Map.fromList
        [ ("viewtemplate", fromString $ show $ viewTemplateToInt vt)
        ]

-- | A single project in PlanMill
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#projects__id__get>
project :: ProjectId -> PlanMill Project
project i = planMillGet $ t "projects" // i

-- | Get a list of teams
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#teams_get>
teams :: PlanMill Teams
teams = planMillPagedGet $ t "teams"

-- | A single team in PlanMill
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#teams__id__get>
team :: TeamId -> PlanMill Team
team i = planMillGet $ t "teams" // i

-- | Get a list of users
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#users_get>
users :: PlanMill Users
users = planMillPagedGet $ t "users"

-- | A single user in PlanMill
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#users__id__get>
user :: UserId -> PlanMill User
user i = planMillGet $ t "users" // i

-- | Update an existing resource user to PlanMill.
--
-- See <https://developers.planmill.com/api/#users__user_id__post>.
editUser :: User -> PlanMill ()
editUser u = planMillPostNoResponse u $ t "users" // u ^. identifier

-- | A single timebalance in PlanMill. This is a read-only item
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#users__id__timebalance_get>
userTimeBalance :: UserId -> PlanMill TimeBalance
userTimeBalance i = planMillGet $ t "users" // i // t "timebalance"

-- | Add a new timereport to PlanMill or update existing (if id is given).
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#timereports_post>
--
-- TODO: separate /create/ and /update/
addTimereport :: NewTimereport -> PlanMill (Inserted Timereport)
addTimereport tr = planMillPost tr $ t "timereports"

-- | Update an existing resource timereport to PlanMill. See timereports/meta
-- for list of filters, columns and formats
--
-- See <http://developers.planmill.com/api/#timereports__timereport_id__post>
--
editTimereport :: EditTimereport -> PlanMill ()
editTimereport tr = planMillPostNoResponse tr $ t "timereports" // tr ^. identifier

-- | Remove a timereport from PlanMill.
--
-- See <http://developers.planmill.com/api/#timereports__timereport_id__delete>
deleteTimereport :: TimereportId -> PlanMill ()
deleteTimereport tid = planMillDeleteNoResponse $ t "timereports" // tid

-- | A single timereport in PlanMill.
--
-- <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#timereports__id__get>
timereport :: TimereportId -> PlanMill Timereport
timereport i = planMillGet $ t "timereports" // i

-- | Get a list of timereports.
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#timereports_get>
timereports :: PlanMill Timereports
timereports = planMillPagedGetQs mempty $ t "timereports"

timereportsFor :: UserId -> PlanMill Timereports
timereportsFor (Ident uid) =
    planMillPagedGetQs qs' $ t "timereports"
  where
    qs' :: QueryString
    qs' = Map.fromList
        [ ("person", fromString $ show uid)
        ]

-- | Get a list of timereports from specified interval.
timereportsFromInterval :: ResultInterval -> PlanMill Timereports
timereportsFromInterval ri =
    planMillPagedGetQs qs $ t "timereports"
  where
    qs = intervalToQueryString ri

timereportsFromIntervalFor :: ResultInterval -> UserId -> PlanMill Timereports
timereportsFromIntervalFor ri (Ident uid) =
    planMillPagedGetQs (qs <> qs') $ t "timereports"
  where
    qs :: QueryString
    qs = intervalToQueryString ri
    qs' :: QueryString
    qs' = Map.fromList
        [ ("person", fromString $ show uid)
        ]

-- | View details of single task.
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#tasks__id__get>
--
-- TODO: seems to return 500 for most tasks
task :: TaskId -> PlanMill Task
task i = planMillGet $ t "tasks" // i

-- | Please note that this data model might change and might be inconsistent with the schema right now.
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#projects__id__tasks_get>
projectTasks :: ProjectId -> PlanMill Tasks
projectTasks pid = planMillPagedGet $ t "projects" // pid // t "tasks"

-- | Get a list of exitcriteria
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#exitcriteria>
exitcriteria :: PlanMill ExitCriteria
exitcriteria = planMillGet $ t "exitcriteria"

-- | Get a list of contacts
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#contacts_get>
contacts :: PlanMill Contacts
contacts = planMillGet $ t "contacts"

-- | Get a list of capacitycalendars
--
-- See <http://developers.planmill.com/api/#capacitycalendars_get>
capacitycalendars :: PlanMill CapacityCalendars
capacitycalendars = planMillGet $ t "capacitycalendars"

-- |
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#users__id__capacity_get>
userCapacity :: Interval Day -> UserId -> PlanMill UserCapacities
userCapacity interval uid = planMillGetQs qs $ t "users" // uid // t "capacity"
  where
    qs = flip elimInterval interval $ \a b -> Map.fromList
        [ ("start",  fromString . showPlanmillUTCTime $ UTCTime a 0)
        , ("finish", fromString . showPlanmillUTCTime $ UTCTime b 0)
        ]

-- | Get a list of vacations.
--
-- See <https://developers.planmill.com/api/#users__user_id__vacations_get>
userVacations :: UserId -> PlanMill Vacations
userVacations uid = planMillPagedGet $ t "users" // uid // t "vacations"

-- | Get a list of assignments
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#projects__id__assignments_get>
projectAssignments :: ProjectId -> PlanMill Assignments
projectAssignments i = planMillGet $ t "projects" // i // t "assignments"

-- | A single user's reportable assignments in PlanMill.
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#users__id__reportableassignments_get>
reportableAssignments :: UserId -> PlanMill ReportableAssignments
reportableAssignments i = planMillPagedGet $ t "users" // i // t "reportableassignments"

-- | Get a list of actions
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#actions_get>
actions :: PlanMill Actions
actions = planMillGet $ t "actions"

-- | View details of single enumeration.
--
-- See <https://online.planmill.com/pmtrial/schemas/v1_5/index.html#enumerations_get>
enumerations :: KnownSymbol k => Proxy k -> PlanMill (EnumDesc k)
enumerations p = planMillGetQs qs $ t "enumerations"
  where
    qs = Map.fromList [ ("name", T.pack $ symbolVal p) ]

-- | Get a list of hooks
--
-- See <https://developers.planmill.com/api/#hooks_get>

hooks :: PlanMill Hooks
hooks = planMillGet $ t "hooks"

-- | Add a new hook
--
-- See <https://developers.planmill.com/api/#hooks__hook_id__post>

addHook :: NewHook -> PlanMill (HookInserted Hook)
addHook h = planMillPost h $ t "hooks"

-- | Remove a hook
--
-- See <https://developers.planmill.com/api/#hooks__hook_id__delete>

deleteHook :: HookId -> PlanMill ()
deleteHook hid = planMillDeleteNoResponse $ t "hooks" // hid

-- | Get a list of reports
--
-- See <https://developers.planmill.com/api/#reports>

reports :: PlanMill ReportsCategories
reports = planMillGet $ t "reports"

-- | Get a spesific report
--
-- See <https://developers.planmill.com/api/#reports__reportName__get>
allRevenuesReport :: PlanMill AllRevenues2
allRevenuesReport = planMillGet $ t "reports" // t "All Revenues 2"

-- | Get the "Value creation per month by employee" report
--
-- See <https://developers.planmill.com/api/#reports__reportName__get>
valueCreationByMonthReport :: Int -> PlanMill PersonValueCreations
valueCreationByMonthReport year = planMillPagedGetQs (Map.fromList [("param1", textShow year)]) $ t "reports" // t "Value creation per month by employee"

-- | Get a list of project's members
--
-- See <https://developers.planmill.com/api/#panel_projects__project_id__members>
projectMembers :: ProjectId -> PlanMill ProjectMembers
projectMembers i = planMillGet $ t "projects" // i // t "members"

-- | Get a hashmap of all possible portfolios
--
-- Uses the project meta field to fetch the values
portfolios :: PlanMill Portfolios
portfolios = planMillGet $ t "projects" // t "meta"
-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

t :: Text -> Text
t = id
