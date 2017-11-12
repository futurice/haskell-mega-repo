{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Reports.API where

import Dashdo.Servant            (DashdoAPI)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Futurice.Report.Columns   (Report)
import Futurice.Servant
import GHC.TypeLits              (KnownSymbol, Symbol)
import Prelude ()
import Servant
import Servant.Chart             (Chart, SVG)
import Servant.Graph             (Graph, ALGA)

import Futurice.App.Reports.Balances          (BalanceReport)
import Futurice.App.Reports.FumFlowdock       (FumFlowdockReport)
import Futurice.App.Reports.FumGithub         (FumGitHubReport)
import Futurice.App.Reports.FumPersonio       (FumPersonioReport)
import Futurice.App.Reports.FumPlanmill       (FumPlanmillReport)
import Futurice.App.Reports.GithubIssues      (IssueReport)
import Futurice.App.Reports.GithubUsers       (GithubUsersReport)
import Futurice.App.Reports.MissingHours
       (MissingHoursReport, MissingHoursTitle, MissingHoursTitleFilt)
import Futurice.App.Reports.PlanmillEmployees (PlanmillEmployeesReport)
import Futurice.App.Reports.PowerAbsences     (PowerAbsenceReport)
import Futurice.App.Reports.PowerProjects     (PowerProjectsReport)
import Futurice.App.Reports.PowerUser         (PowerUserReport)
import Futurice.App.Reports.TimereportsByTask (TimereportsByTaskReport)

type ReportTypes = '[HTML, CSV, JSON]

data R (path :: Symbol) (report :: *)

type Reports =
    '[ R "issues"             IssueReport
    , R "fum-github"          FumGitHubReport
    , R "fum-flowdock"        FumFlowdockReport
    , R "fum-planmill"        FumPlanmillReport
    , R "fum-personio"        FumPersonioReport
    , R "github-users"        GithubUsersReport
    , R "missing-hours"       (MissingHoursReport MissingHoursTitle)
    , R "missing-hours-filt"  (MissingHoursReport MissingHoursTitleFilt)
    , R "balances"            BalanceReport
    , R "hours-by-task"       TimereportsByTaskReport
    , R "planmill-employees"  PlanmillEmployeesReport
    ]

-- | This, 'RReport' and 'RName', type families are needed to make 'FoldReportsAPI' reduce
-- to the ':<|>' in cons case.
type family RPath r where
    RPath (R path report) = path

type family RReport r where
    RReport (R path report) = report

type family RName r where
    RName (R path (Report name params a)) = name

class (KnownSymbol (RPath r), KnownSymbol (RName r), NFData (RReport r)) => RClass r
instance (KnownSymbol path, KnownSymbol name, NFData params, NFData a)
    => RClass (R path (Report name params a))

type family FoldReportsAPI rs :: * where
    FoldReportsAPI '[]       = Get '[HTML] (HtmlPage "index")
    FoldReportsAPI (r ': rs) =
        RPath r :> Get ReportTypes (RReport r) :<|>
        RPath r :> "json" :> Get '[JSON] (RReport r) :<|>
        RPath r :> "csv" :> Get '[CSV] (RReport r) :<|>
        FoldReportsAPI rs

type ReportsAPI = FoldReportsAPI Reports
    -- Charts
    :<|> "charts" :> "utz" :> Get '[SVG] (Chart "utz")
    :<|> "charts" :> "missing-hours" :> Get '[SVG] (Chart "missing-hours")
    -- Graphs
    :<|> "graphs" :> "supervisors" :> Get '[ALGA] (Graph "supervisors")
    -- Additional non-reports
    :<|> "power" :> "users" :> Get '[JSON] PowerUserReport
    :<|> "power" :> "projects" :> Get '[JSON] PowerProjectsReport
    :<|> "power" :> "absences" :> QueryParam "month" Month :> Get '[JSON] PowerAbsenceReport
    :<|> "dashdo" :> DashdoAPI

reportsApi :: Proxy ReportsAPI
reportsApi = Proxy
