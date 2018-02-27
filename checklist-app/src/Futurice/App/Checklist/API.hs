{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.Checklist.API where

import Futurice.Prelude
import Prelude ()

import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Servant          (SSOUser)
import Servant.API
import Servant.Chart             (Chart, SVG)
import Servant.HTML.Lucid        (HTML)

import Futurice.App.Checklist.Ack     (Ack)
import Futurice.App.Checklist.Command (Command)
import Futurice.App.Checklist.Types
       (Checklist, Employee, Identifier, Office, SortCriteria, Task, TaskRole)

import qualified Personio

type ChecklistAPI = IndexPageEndpoint
    -- Collections
    :<|> TasksPageEndpoint
    :<|> ChecklistsPageEndpoint
    -- New
    :<|> CreateChecklistPageEndpoint
    :<|> CreateTaskPageEndpoint
    :<|> CreateEmployeePageEndpoint
    -- Items
    :<|> ChecklistPageEndpoint
    :<|> TaskPageEndpoint
    :<|> EmployeePageEndpoint
    :<|> EmployeeAuditPageEndpoint
    -- Archive
    :<|> ArchivePageEndpoint
    -- Personio
    :<|> PersonioPageEndpoint
    -- Report(s)
    :<|> ReportPageEndpoint
    :<|> "reports" :> "charts" :> "done.svg" :> SSOUser :> Get '[SVG] (Chart "done")
    -- Help
    :<|> ApplianceHelpEndpoint
    -- Command
    :<|> "command" :> SSOUser :> ReqBody '[JSON] (Command Proxy) :> Post '[JSON] Ack
    :<|> StatsPageEndpoint

checklistApi :: Proxy ChecklistAPI
checklistApi = Proxy

-------------------------------------------------------------------------------
-- Collections
-------------------------------------------------------------------------------

type IndexPageEndpoint =
    SSOUser :>
    QueryParam "location" Office :>
    QueryParam "checklist" (Identifier Checklist) :>
    QueryParam "task" (Identifier Task) :>
    QueryFlag "show-done" :>
    QueryFlag "show-old" :>
    Get '[HTML] (HtmlPage "indexpage")

type TasksPageEndpoint =
    "tasks" :>
    SSOUser :>
    QueryParam "role" TaskRole :>
    QueryParam "checklist" (Identifier Checklist) :>
    Get '[HTML] (HtmlPage "tasks")

type ChecklistsPageEndpoint =
    "checklists" :>
    SSOUser :>
    Get '[HTML] (HtmlPage "checklists")

-------------------------------------------------------------------------------
-- New
-------------------------------------------------------------------------------

type CreateChecklistPageEndpoint =
    SSOUser :>
    "checklists" :>
    "create" :>
    Get '[HTML] (HtmlPage "create-checklist")

type CreateTaskPageEndpoint =
    SSOUser :>
    "tasks" :>
    "create" :>
    Get '[HTML] (HtmlPage "create-task")

type CreateEmployeePageEndpoint =
    SSOUser :>
    "employees" :>
    "create" :>
    QueryParam "copy-employee" (Identifier Employee) :>
    QueryParam "personio-id" Personio.EmployeeId :>
    QueryFlag "leaving-employee" :>
    Get '[HTML] (HtmlPage "create-employee")

-------------------------------------------------------------------------------
-- Items
-------------------------------------------------------------------------------

type ChecklistPageEndpoint =
    SSOUser :>
    "checklists" :>
    Capture "checklist-id" (Identifier Checklist) :>
    Get '[HTML] (HtmlPage "checklist")

type TaskPageEndpoint =
    SSOUser :>
    "tasks" :>
    Capture "task-id" (Identifier Task) :>
    Get '[HTML] (HtmlPage "task")

type EmployeePageEndpoint =
    SSOUser :>
    "employees" :>
    Capture "employee-id" (Identifier Employee) :>
    Get '[HTML] (HtmlPage "employee")

type EmployeeAuditPageEndpoint =
    SSOUser :>
    "employees" :>
    Capture "employee-id" (Identifier Employee) :>
    "audit" :>
    Get '[HTML] (HtmlPage "employee-audit")

-------------------------------------------------------------------------------
-- Personio
-------------------------------------------------------------------------------

type PersonioPageEndpoint =
    SSOUser :>
    "personio" :>
    Get '[HTML] (HtmlPage "personio")

-------------------------------------------------------------------------------
-- Archive
-------------------------------------------------------------------------------

type ArchivePageEndpoint =
    SSOUser :>
    "archive" :>
    Get '[HTML] (HtmlPage "archive")

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

type ReportPageEndpoint =
    SSOUser :>
    "report" :>
    QueryParam "checklist" (Identifier Checklist) :>
    QueryParam "day-from" Day :>
    QueryParam "day-to" Day :>
    Get '[HTML] (HtmlPage "report")

-------------------------------------------------------------------------------
-- Help
-------------------------------------------------------------------------------

type ApplianceHelpEndpoint =
    SSOUser :>
    "help" :>
    "appliance" :>
    Get '[HTML] (HtmlPage "appliance-help")

-------------------------------------------------------------------------------
-- Stats
-------------------------------------------------------------------------------

type StatsPageEndpoint =
    SSOUser :>
    "stats" :>
    QueryParam' '[Required] "sort-criteria" SortCriteria :>
    QueryFlag "sort-desc" :>
    Get '[HTML] (HtmlPage "stats")

-------------------------------------------------------------------------------
-- Proxies
-------------------------------------------------------------------------------

indexPageEndpoint :: Proxy IndexPageEndpoint
indexPageEndpoint = Proxy

tasksPageEndpoint :: Proxy TasksPageEndpoint
tasksPageEndpoint = Proxy

checklistsPageEndpoint :: Proxy ChecklistsPageEndpoint
checklistsPageEndpoint = Proxy

createChecklistPageEndpoint :: Proxy CreateChecklistPageEndpoint
createChecklistPageEndpoint = Proxy

createTaskPageEndpoint :: Proxy CreateTaskPageEndpoint
createTaskPageEndpoint = Proxy

createEmployeePageEndpoint :: Proxy CreateEmployeePageEndpoint
createEmployeePageEndpoint = Proxy

checklistPageEndpoint :: Proxy ChecklistPageEndpoint
checklistPageEndpoint = Proxy

taskPageEndpoint :: Proxy TaskPageEndpoint
taskPageEndpoint = Proxy

employeePageEndpoint :: Proxy EmployeePageEndpoint
employeePageEndpoint = Proxy

employeeAuditPageEndpoint :: Proxy EmployeeAuditPageEndpoint
employeeAuditPageEndpoint = Proxy

applianceHelpEndpoint :: Proxy ApplianceHelpEndpoint
applianceHelpEndpoint = Proxy

archivePageEndpoint :: Proxy ArchivePageEndpoint
archivePageEndpoint = Proxy

personioPageEndpoint :: Proxy PersonioPageEndpoint
personioPageEndpoint = Proxy

reportPageEndpoint :: Proxy ReportPageEndpoint
reportPageEndpoint = Proxy

statsPageEndpoint :: Proxy StatsPageEndpoint
statsPageEndpoint = Proxy
