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

import Futurice.App.Reports.ActiveAccounts            (ActiveAccounts)
import Futurice.App.Reports.ActiveSubcontractors      (ActiveSubcontractorData)
import Futurice.App.Reports.DoWeStudy                 (DoWeStudyData, StudyKind)
import Futurice.App.Reports.IDontKnow                 (IDontKnowData)
import Futurice.App.Reports.Inventory                 (InventorySummary)
import Futurice.App.Reports.MissingHours              (MissingHoursReport)
import Futurice.App.Reports.OfficeVibeIntegration
       (OfficeVibeGroup, OfficeVibeRelation, OfficeVibeUser)
import Futurice.App.Reports.PlanMillAccountValidation (PMAccountValidation)
import Futurice.App.Reports.PowerAbsences             (PowerAbsenceReport)
import Futurice.App.Reports.PowerProjects             (PowerProjectsReport)
import Futurice.App.Reports.PowerUser                 (PowerUserReport)
import Futurice.App.Reports.ProjectHours              (ProjectHoursData)
import Futurice.App.Reports.SupervisorsGraph          (Emp)
import Futurice.App.Reports.TimereportsByTask         (TimereportsByTaskReport)
import Futurice.App.Reports.TimereportsDump           (SimpleTimereport)

type ReportTypes = '[HTML, CSV, JSON]

data Record route = Record
    { recIndex :: route :- Get '[HTML] (HtmlPage "index")

    -- "legacy" reports
    , recMissingHours :: route :- "missing-hours" :> Get ReportTypes MissingHoursReport
    , recHoursByTask  :: route :- "hours-by-task" :> Get ReportTypes TimereportsByTaskReport

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

    -- missing hours notification
    , recCommandMissingHoursNotification :: route :- "command" :> "send-missing-hours-notification" :> Post '[JSON] Text

    -- futuqu
    , recFutuqu :: route :- "futuqu" :> FutuquAPI

    -- dashdo
    , recDashdo :: route :- "dashdo" :> DashdoAPI
    }
  deriving Generic


type ReportsAPI = ToServantApi Record

reportsApi :: Proxy ReportsAPI
reportsApi = genericApi (Proxy :: Proxy Record)
