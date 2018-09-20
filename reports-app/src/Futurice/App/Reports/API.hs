{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Servant.Graph             (ALGA, Graph)

import Futurice.App.Reports.ActiveAccounts            (ActiveAccounts)
import Futurice.App.Reports.Inventory                 (InventorySummary)
import Futurice.App.Reports.MissingHours              (MissingHoursReport)
import Futurice.App.Reports.PlanMillAccountValidation (PMAccountValidation)
import Futurice.App.Reports.PowerAbsences             (PowerAbsenceReport)
import Futurice.App.Reports.PowerProjects             (PowerProjectsReport)
import Futurice.App.Reports.PowerUser                 (PowerUserReport)
import Futurice.App.Reports.SupervisorsGraph          (Emp)
import Futurice.App.Reports.TimereportsByTask         (TimereportsByTaskReport)
import Futurice.App.Reports.TimereportsDump           (SimpleTimereport)

type ReportTypes = '[HTML, CSV, JSON]

data R (path :: Symbol) (report :: *)

type Reports =
    '[R "missing-hours"       MissingHoursReport
    , R "hours-by-task"       TimereportsByTaskReport
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
    -- Tables
    :<|> "tables" :> "active-accounts"      :> Get '[HTML] ActiveAccounts
    :<|> "tables" :> "active-accounts.json" :> Get '[JSON] ActiveAccounts
    :<|> "tables" :> "planmill-account-validation" :> Get '[HTML] PMAccountValidation
    :<|> "tables" :> "inventory-summary"    :> Get '[HTML] InventorySummary
    -- Dump
    :<|> "dump" :> "timereports.csv.xz" :> Get '[LZMA CSV] [SimpleTimereport]
    -- Charts
    :<|> "charts" :> "utz" :> Get '[SVG] (Chart "utz")
    :<|> "charts" :> "missing-hours" :> Get '[SVG] (Chart "missing-hours")
    :<|> "charts" :> "missing-hours-daily" :> Get '[SVG] (Chart "missing-hours-daily")
    :<|> "charts" :> "career-length" :> Get '[SVG] (Chart "career-length")
    :<|> "charts" :> "career-length-relative" :> Get '[SVG] (Chart "career-length-relative")
    :<|> "charts" :> "inventory-quantiles" :> Get '[SVG] (Chart "inventory-quantiles")
    -- Graphs
    :<|> "graphs" :> "supervisors" :> Get '[ALGA] (Graph Emp "supervisors")
    -- Additional non-reports
    :<|> "power" :> "users" :> Get '[JSON] PowerUserReport
    :<|> "power" :> "projects" :> Get '[JSON] PowerProjectsReport
    :<|> "power" :> "absences" :> QueryParam "month" Month :> Get '[JSON] PowerAbsenceReport
    -- missing hours notification
    :<|> "command" :> "send-missing-hours-notification" :> Post '[JSON] Text
    -- dashdo
    :<|> "dashdo" :> DashdoAPI

reportsApi :: Proxy ReportsAPI
reportsApi = Proxy
