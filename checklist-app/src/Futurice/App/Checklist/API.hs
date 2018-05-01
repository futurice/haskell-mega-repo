{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.Checklist.API where

import Futurice.Prelude
import Prelude ()

import Futurice.App.Checklist.Ack     (Ack)
import Futurice.App.Checklist.Command (Command)
import Futurice.App.Checklist.Types
       (ChecklistId, Employee, Identifier, Office, SortCriteria, Task,
       TaskNode, TaskRole)
import Futurice.Lucid.Foundation      (HtmlPage)
import Futurice.Servant               (SSOUser)
import Servant.API
import Servant.Chart                  (Chart, SVG)
import Servant.Graph                  (ALGAPNG, Graph)
import Servant.HTML.Lucid             (HTML)

import qualified Personio

type ChecklistAPI = IndexPageEndpoint
    -- Collections
    :<|> TasksPageEndpoint
    :<|> ChecklistsPageEndpoint
    -- New
    :<|> CreateTaskPageEndpoint
    :<|> CreateEmployeePageEndpoint
    -- Items
    :<|> ChecklistPageEndpoint
    :<|> ChecklistGraphEndpoint
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
    :<|> ServicesHelpEndpoint
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
    QueryParam "checklist" ChecklistId :>
    QueryParam "task" (Identifier Task) :>
    QueryFlag "show-done" :>
    QueryFlag "show-old" :>
    Get '[HTML] (HtmlPage "indexpage")

type TasksPageEndpoint =
    "tasks" :>
    SSOUser :>
    QueryParam "role" TaskRole :>
    QueryParam "checklist" ChecklistId :>
    Get '[HTML] (HtmlPage "tasks")

type ChecklistsPageEndpoint =
    "checklists" :>
    SSOUser :>
    Get '[HTML] (HtmlPage "checklists")

-------------------------------------------------------------------------------
-- New
-------------------------------------------------------------------------------

type CreateTaskPageEndpoint =
    SSOUser :>
    "tasks" :>
    "create" :>
    Get '[HTML] (HtmlPage "create-task")

type CreateEmployeePageEndpoint =
    SSOUser :>
    "employees" :>
    "create" :>
    QueryParam "checklist" ChecklistId :>
    QueryParam "copy-employee" (Identifier Employee) :>
    QueryParam "personio-id" Personio.EmployeeId :>
    Get '[HTML] (HtmlPage "create-employee")

-------------------------------------------------------------------------------
-- Items
-------------------------------------------------------------------------------

type ChecklistPageEndpoint =
    SSOUser :>
    "checklists" :>
    Capture "checklist-id" ChecklistId :>
    Get '[HTML] (HtmlPage "checklist")

type ChecklistGraphEndpoint =
    SSOUser :>
    "checklists" :>
    Capture "checklist-id" ChecklistId :>
    "task-graph.png" :>
    Get '[ALGAPNG] (Graph TaskNode "checklist")

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
    QueryParam "checklist" ChecklistId :>
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

type ServicesHelpEndpoint =
    SSOUser :>
    "help" :>
    "services" :>
    Get '[HTML] (HtmlPage "services-help")

-------------------------------------------------------------------------------
-- Stats
-------------------------------------------------------------------------------

type StatsPageEndpoint =
    SSOUser :>
    "stats" :>
    QueryParam' '[Required] "sort-criteria" SortCriteria :>
    QueryFlag "sort-desc" :>
    QueryFlag "show-all" :>
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

createTaskPageEndpoint :: Proxy CreateTaskPageEndpoint
createTaskPageEndpoint = Proxy

createEmployeePageEndpoint :: Proxy CreateEmployeePageEndpoint
createEmployeePageEndpoint = Proxy

checklistPageEndpoint :: Proxy ChecklistPageEndpoint
checklistPageEndpoint = Proxy

checklistGraphEndpoint :: Proxy ChecklistGraphEndpoint
checklistGraphEndpoint = Proxy

taskPageEndpoint :: Proxy TaskPageEndpoint
taskPageEndpoint = Proxy

employeePageEndpoint :: Proxy EmployeePageEndpoint
employeePageEndpoint = Proxy

employeeAuditPageEndpoint :: Proxy EmployeeAuditPageEndpoint
employeeAuditPageEndpoint = Proxy

applianceHelpEndpoint :: Proxy ApplianceHelpEndpoint
applianceHelpEndpoint = Proxy

servicesHelpEndpoint :: Proxy ServicesHelpEndpoint
servicesHelpEndpoint = Proxy

archivePageEndpoint :: Proxy ArchivePageEndpoint
archivePageEndpoint = Proxy

personioPageEndpoint :: Proxy PersonioPageEndpoint
personioPageEndpoint = Proxy

reportPageEndpoint :: Proxy ReportPageEndpoint
reportPageEndpoint = Proxy

statsPageEndpoint :: Proxy StatsPageEndpoint
statsPageEndpoint = Proxy
