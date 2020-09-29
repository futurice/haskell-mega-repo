{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Reports.Markup (indexPage) where

import Futuqu
import Futurice.Constants        (servicePublicUrl)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Futurice.Services         (Service (..))
import Futurice.Time.Month
import Lucid.Base                (Attribute (..))
import Lucid.Servant             (linkAbsHref_)
import Prelude ()
import Servant.Links             (allFieldLinks')

import Futurice.App.Reports.API

attrValue :: Functor f => LensLike' f Attribute Text
attrValue f (Attribute ak av) = Attribute ak <$> f av

indexPage :: Day -> HtmlPage "index"
indexPage today = page_ "Reports" $ do
    let month = dayToMonth today

    row_ $ large_ 12 $ h1_ "Reports"

    fullRow_ $ h2_ "Futurice queries"
    fullRow_ $ ul_ $ do
        let hrefs = allFieldLinks' linkAbsHref_
        let makeLi :: Attribute -> Text -> Html ()
            makeLi attr desc = li_ $ do
                a_ [ attr & attrValue %~ \av -> "/futuqu" <> av ] $ toHtml desc
                " ("
                a_ [ attr & attrValue %~ \av -> "/futuqu" <> av <> ".json" ] $ "json"
                ", "
                a_ [ attr & attrValue %~ \av -> "/futuqu" <> av <> ".csv" ] $ "csv"
                ")"

        makeLi (futuquRoutePeople hrefs) "List of all people"
        makeLi (futuquRouteAccounts hrefs) "List of all accounts"
        makeLi (futuquRouteProjects hrefs []) "List of all projects"
        makeLi (futuquRouteTasks hrefs [] []) "List of all tasks"
        makeLi (futuquRouteCapacities hrefs month) "(Non-zero) daily capacities for a month"
        makeLi (futuquRouteTimereports hrefs month) "Timereports for a month"
        makeLi (futuquRouteMissingHours hrefs month) "Example report: missing hours"
        makeLi (futuquRouteHourKinds hrefs month) "Example report: People hours aggregated by kind: billable, non-billable etc"
        li_ $ a_ [ futuquRouteTimereportsStream hrefs Nothing & attrValue %~ \av -> "/futuqu" <> av ] "Streaming: all timereports csv"

    fullRow_ $ h2_ "Dashdo"
    fullRow_ $ ul_ $ do
        li_ $ do
            a_ [ href_ "/dashdo/" ] "Dashdo dashboards"
            ", including missing-hours, balances, and other"

    fullRow_ $ h2_ "Charts"
    fullRow_ $ ul_ $ do
        li_ $ a_ [ recordHref_ recChartsCareerLength         ] "Distribution of career lengths over time, absolute"
        li_ $ a_ [ recordHref_ recChartsCareerLengthRelative ] "Distribution of career length over time, relative"

        li_ $ a_ [ recordHref_ recChartsMissingHours      ] "Missing hours by tribe per employee per week"
        li_ $ a_ [ recordHref_ recChartsMissingHoursDaily ] "Missing hours total per day"
        li_ $ a_ [ recordHref_ recChartsUtz               ] "Company UTZ"

    fullRow_ $ h2_ "Graphs"
    fullRow_ $ ul_ $ do
        li_ $ a_ [ recordHref_ recGraphsSupervisors ] "Supervisor graph"

    fullRow_ $ h2_ "Tables"
    fullRow_ $ ul_ $ do
        li_ $ a_ [ recordHref_ recMissingHoursSimplified Nothing Nothing Nothing] "Missing hours"
        li_ $ a_ [ recordHref_ recMissingHoursByProject Nothing Nothing] "Missing hours by project"
        li_ $ a_ [ recordHref_ recMissingHours ] "Missing hours extended representation"
        li_ $ a_ [ recordHref_ recHoursByTask  ] "Hours by task"

        li_ $ a_ [ recordHref_ recTablesActiveAccounts            ] "Active accounts"
        li_ $ a_ [ recordHref_ recTablesPMAccountValidation       ] "Validation of PlanMill account data"
        li_ $ a_ [ recordHref_ recTablesInventorySummary          ] "Mobile budget stats"
        li_ $ a_ [ recordHref_ recTablesProjectHoursData          ] "Hours by project and type"
        li_ $ a_ [ recordHref_ recTablesIDontKnow Nothing Nothing ] "I don't know... report"
        li_ $ a_ [ recordHref_ recTablesDoWeStudy Nothing Nothing Nothing ] "Learning hours report"
        li_ $ a_ [ recordHref_ recActiveSubcontractors Nothing    ] "Active subcontractors"
        li_ $ a_ [ recordHref_ recOKRCompetences ] "OKR competences report"

    fullRow_ $ h2_ "Integrations for Power"
    fullRow_ $ ul_ $ do
        li_ $ a_ [ recordHref_ recPowerUsers            ] "Users"
        li_ $ a_ [ recordHref_ recPowerProjects         ] "Projects"
        li_ $ a_ [ recordHref_ recPowerAbsences Nothing ] "Absences"
        li_ $ a_ [ recordHref_ recPowerAllRevenueReport Nothing ] "All Revenues 2 report"
        li_ $ a_ [ recordHref_ recPowerUTZ Nothing ] "UTZ"

    fullRow_ $ h2_ "Swagger UI"
    fullRow_ $ ul_ $ do
        li_ $ a_ [ href_ "/swagger-ui" ] "Swagger-UI"

    fullRow_ $ h2_ "Reports and charts elsewhere"
    fullRow_ $ do
        p_ "Some links require superpowers"

        ul_ $ do
            elink_ HCService "/personio-validation"
                "Personio data validation: empty fields etc."
            elink_ FumCarbonService "/reports/compare-old-fum"
                "Personio ↔ FUM5: check emails and phone numbers"
            elink_ PlanmillSyncService "/"
                "Personio → PlanMill: data agree"
            elink_ GithubSyncService "/"
                "Personio → GitHub: data agree"
            elink_ PersonioProxyService "/charts/employees.svg"
                "Personio active employees chart"
            elink_ SmileysApiService "/charts/absolute.svg"
                "Smileys absolute count chart"
  where
    elink_ :: Monad m => Service -> Text -> Text -> HtmlT m ()
    elink_ service href description = li_ $ do
        a_ [ href_ href' ] (toHtml href')
        toHtmlRaw (" &ndash; " :: String)
        toHtml description
      where
        href' =  servicePublicUrl service <> href
