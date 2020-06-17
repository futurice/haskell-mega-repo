{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Reports.API where

import Dashdo.Servant            (DashdoAPI)
import Futuqu.API
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Futurice.Servant
import Futurice.Tribe            (Tribe)
import Prelude ()
import Servant
import Servant.API.Generic
import Servant.Cached
import Servant.Chart             (Chart, SVG)
import Servant.Graph             (ALGA, Graph)

import Futurice.App.Reports.ActiveAccounts              (ActiveAccounts)
import Futurice.App.Reports.ActiveSubcontractorsByHours
       (ActiveSubcontractorData)
import Futurice.App.Reports.Capacity                    (Capacity)
import Futurice.App.Reports.DoWeStudy
       (DoWeStudyData, StudyKind)
import Futurice.App.Reports.IDontKnow                   (IDontKnowData)
import Futurice.App.Reports.Inventory                   (InventorySummary)
import Futurice.App.Reports.MissingHours
       (MissingHoursReport, MissingHoursSimplifiedReport)
import Futurice.App.Reports.OfficeVibeIntegration
       (OfficeVibeGroup, OfficeVibeRelation, OfficeVibeUser)
import Futurice.App.Reports.OKRCompetencies             (CompetencyReport)
import Futurice.App.Reports.PlanMillAccountValidation   (PMAccountValidation)
import Futurice.App.Reports.PowerAbsences               (PowerAbsenceReport)
import Futurice.App.Reports.PowerAllRevenues            (PowerAllRevenues)
import Futurice.App.Reports.PowerProjects               (PowerProjectsReport)
import Futurice.App.Reports.PowerUser                   (PowerUserReport)
import Futurice.App.Reports.PowerUTZ                    (PowerUTZReport)
import Futurice.App.Reports.ProjectHours                (ProjectHoursData)
import Futurice.App.Reports.ProjectMembers              (ProjectMembers)
import Futurice.App.Reports.SupervisorsGraph            (Emp)
import Futurice.App.Reports.TeamsHoursByCategoryReport
       (TeamsHoursByCategoryReport)
import Futurice.App.Reports.TimereportsByTask
       (TimereportsByTaskReport)
import Futurice.App.Reports.TimereportsDump             (SimpleTimereport)
import Futurice.App.Reports.ValueCreation               (ValueCreationReport)

import qualified FUM.Types.Login as FUM
import qualified Personio        as P

type ReportTypes = '[HTML, CSV, JSON]

data Record route = Record
    { recIndex :: route :- Get '[HTML] (HtmlPage "index")

    -- "legacy" reports
    , recMissingHours :: route :- "missing-hours" :> Get ReportTypes MissingHoursReport
    , recHoursByTask  :: route :- "hours-by-task" :> Get ReportTypes TimereportsByTaskReport
    , recMissingHoursSimplified :: route :- "missing-hours-simplified" :> QueryParam' '[Lenient, Optional] "employee" Text :> QueryParam' '[Lenient, Optional] "month" Month :> QueryParam' '[Lenient, Optional] "tribe" Tribe :> Get '[HTML] MissingHoursSimplifiedReport

    -- Tables
    , recTablesActiveAccounts       :: route :- "tables" :> "active-accounts"      :> Get '[HTML] ActiveAccounts
    , recTablesActiveAccountsJSON   :: route :- "tables" :> "active-accounts.json" :> Get '[JSON] ActiveAccounts
    , recTablesPMAccountValidation  :: route :- "tables" :> "planmill-account-validation" :> Get '[HTML] PMAccountValidation
    , recTablesInventorySummary     :: route :- "tables" :> "inventory-summary"    :> Get '[HTML] InventorySummary
    , recTablesProjectHoursData     :: route :- "tables" :> "project-hours"        :> Get '[HTML] ProjectHoursData
    , recTablesProjectHoursDataJSON :: route :- "tables" :> "project-hours.json"   :> Get '[JSON] ProjectHoursData
    , recTablesIDontKnow            :: route :- "tables" :> "i-dont-know"          :> QueryParam "month" Month :> QueryParam' '[Lenient, Optional] "tribe" Tribe :> Get '[HTML] IDontKnowData
    , recTablesDoWeStudy            :: route :- "tables" :> "do-we-study"          :> QueryParam' '[Lenient, Optional] "studykind" StudyKind :> QueryParam "month" Month :> QueryParam' '[Lenient, Optional] "tribe" Tribe :> Get '[HTML] DoWeStudyData
    , recActiveSubcontractors       :: route :- "tables" :> "active-subcontractors" :> QueryParam "day" Day :> Get '[HTML] ActiveSubcontractorData
    , recOKRCompetences             :: route :- "tables" :> "multdisciplinary-projects-count"      :> Get '[HTML] CompetencyReport
    , recOKRCompetencesJSON         :: route :- "tables" :> "multdisciplinary-projects-count.json" :> Get '[JSON] CompetencyReport

    -- Officevibe
    , recOfficevibeUsers         :: route :- "officevibe" :> "users.csv" :> Get '[CSV] [OfficeVibeUser]
    , recOfficevibeGroups        :: route :- "officevibe" :> "groups.csv" :> Get '[CSV] [OfficeVibeGroup]
    , recOfficevibeGroupsMapping :: route :- "officevibe" :> "groups-mapping.csv" :> Get '[CSV] [OfficeVibeRelation]

    -- Dump: can be deleted, we have futuqu
    , recTimereportsDump :: route :- "dump" :> "timereports.csv.xz" :> CachedGet (LZMA CSV) [SimpleTimereport]

    -- Charts
    , recChartsUtz                  :: route :- "charts" :> "utz" :> Get '[SVG] (Chart "utz")
    , recChartsMissingHours         :: route :- "charts" :> "missing-hours" :> Get '[SVG] (Chart "missing-hours")
    , recChartsMissingHoursDaily    :: route :- "charts" :> "missing-hours-daily" :> Get '[SVG] (Chart "missing-hours-daily")
    , recChartsCareerLength         :: route :- "charts" :> "career-length" :> Get '[SVG] (Chart "career-length")
    , recChartsCareerLengthRelative :: route :- "charts" :> "career-length-relative" :> Get '[SVG] (Chart "career-length-relative")
    , recChartsInventoryQuantiles   :: route :- "charts" :> "inventory-quantiles" :> Get '[SVG] (Chart "inventory-quantiles")

    -- Graphs
    , recGraphsSupervisors :: route :- "graphs" :> "supervisors" :> Get '[ALGA] (Graph Emp "supervisors")

    -- Additional non-reports
    , recPowerUsers    :: route :- "power" :> "users" :> Get '[JSON] PowerUserReport
    , recPowerProjects :: route :- "power" :> "projects" :> Get '[JSON] PowerProjectsReport
    , recPowerAbsences :: route :- "power" :> "absences" :> QueryParam "month" Month :> Get '[JSON] PowerAbsenceReport
    , recPowerAllRevenueReport :: route :- "power" :> "all-revenue" :> QueryParam "month" Month :> Get '[JSON] PowerAllRevenues
    , recPowerUTZ      :: route :- "power" :> "utz" :> QueryParam "month" Month :> Get '[JSON] PowerUTZReport
    , recComputers     :: route :- "computers" :> Get '[JSON] (Map P.EmployeeId [Text])

    -- For Bubbleburster
    , recProjectMembers :: route :- "project" :> "members" :> Get '[JSON] [ProjectMembers]

    -- For Data lake
    , recValueCreation :: route :- "report" :> "value-creation" :> QueryParam "year" Integer :> Get '[JSON] ValueCreationReport

    , recTeamsHoursByCategory :: route :- "report" :> "teams-hours-by-category" :> QueryParam "start" Day :> QueryParam "end" Day :> Get '[JSON] TeamsHoursByCategoryReport

    -- missing hours notification
--    , recCommandMissingHoursNotification :: route :- "command" :> "send-missing-hours-notification" :> Post '[JSON] Text

    -- futuqu
    , recFutuqu :: route :- "futuqu" :> FutuquAPI

    -- dashdo
    , recDashdo :: route :- "dashdo" :> DashdoAPI

    -- Peakon
    , recPeakonEngagementOverview :: route :- "peakon" :> "engagement" :> "overview" :> Get '[JSON] Value
    , recPeakonEngagementDrivers  :: route :- "peakon" :> "engagement" :> "drivers"  :> Get '[JSON] Value
    , recPeakonSegments           :: route :- "peakon" :> "segments" :> Get '[JSON] Value

    -- For futulog
    , recFumCapacity :: route :- "report" :> "capacity" :> Capture "login" FUM.Login :> Capture "month" Month :> Get '[JSON] [Capacity]
    }
  deriving Generic


type ReportsAPI = ToServantApi Record

reportsApi :: Proxy ReportsAPI
reportsApi = genericApi (Proxy :: Proxy Record)
