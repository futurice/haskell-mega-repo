{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
module Futurice.App.Reports (defaultMain) where

import Control.Lens               (each)
import Futurice.Integrations
       (Integrations, beginningOfPrev2Month, endOfPrevMonth, previousFriday)
import Futurice.Metrics.RateMeter (mark)
import Futurice.Periocron
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Report.Columns    (reportParams)
import Futurice.Servant
import Futurice.Time              (unNDT)
import Generics.SOP               (All, hcmap, hcollapse)
import GHC.TypeLits               (KnownSymbol, symbolVal)
import Numeric.Interval.NonEmpty  ((...))
import Prelude ()
import Servant
import Servant.Chart              (Chart (..))
import Servant.Graph              (Graph (..))

import Futurice.App.Reports.ActiveAccounts
import Futurice.App.Reports.API
import Futurice.App.Reports.CareerLengthChart
       (careerLengthData, careerLengthRelativeRender, careerLengthRender)
import Futurice.App.Reports.Config
import Futurice.App.Reports.Ctx
import Futurice.App.Reports.Dashdo                    (makeDashdoServer)
import Futurice.App.Reports.Markup
import Futurice.App.Reports.MissingHours
       (MissingHoursReport, mhpTotalHours, missingHoursEmployeePredicate,
       missingHoursReport)
import Futurice.App.Reports.MissingHoursChart
       (MissingHoursChartData, missingHoursChartData, missingHoursChartRender)
import Futurice.App.Reports.MissingHoursDailyChart
       (missingHoursDailyChartData, missingHoursDailyChartRender)
import Futurice.App.Reports.MissingHoursNotifications
import Futurice.App.Reports.PowerAbsences
       (PowerAbsenceReport, powerAbsenceReport)
import Futurice.App.Reports.PowerProjects
       (PowerProjectsReport, powerProjectsReport)
import Futurice.App.Reports.PowerUser
       (PowerUserReport, powerUserReport)
import Futurice.App.Reports.SupervisorsGraph          (supervisorsGraph)
import Futurice.App.Reports.TimereportsByTask
       (TimereportsByTaskReport, timereportsByTaskReport)
import Futurice.App.Reports.UtzChart
       (utzChartData, utzChartRender)

newtype ReportEndpoint r = ReportEndpoint (Ctx -> IO (RReport r))

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

-- Note: we cachedIO with () :: () as a key. It's ok as 'Cache'
-- uses both @key@ and @value@ TypeRep's as key to non-typed map.

serveMissingHoursReport :: Ctx -> IO MissingHoursReport
serveMissingHoursReport ctx = cachedIO' ctx () $ do
    day <- currentDay
    let interval = beginningOfPrev2Month day ... previousFriday day
    runIntegrations' ctx (missingHoursReport missingHoursEmployeePredicate interval)

missingHoursStats :: Ctx -> IO ()
missingHoursStats ctx = runLogT "missing-hours-series" lgr $ do
    (results, today) <- liftIO $ runIntegrations' ctx $ do
        today <- currentDay
        let monthsI    = beginningOfPrev2Month today ... endOfPrevMonth today
        let fridayI    = beginningOfPrev2Month today ... previousFriday today
        let yesterdayI = beginningOfPrev2Month today ... pred today

        results <- each (missingHoursReport predicate)
            (monthsI, fridayI, yesterdayI)

        return (results, today)

    let totals = over each
            (\r -> realToFrac $ unNDT $ r ^. reportParams . mhpTotalHours :: Double)
            results

    void $ safePoolExecute ctx insertQuery (today, totals ^. _1, totals ^._2, totals ^. _3)
  where
    lgr = ctxLogger ctx
    predicate = missingHoursEmployeePredicate

    insertQuery = fromString $ unwords
        [ "INSERT INTO reports.missing_hours as c (day, full_months, last_friday, yesterday)"
        , "VALUES (?, ?, ?, ?) ON CONFLICT (day) DO UPDATE"
        , "SET last_friday = LEAST(c.last_friday, EXCLUDED.last_friday),"
        , "    yesterday = LEAST(c.yesterday, EXCLUDED.yesterday),"
        , "    full_months = LEAST(c.full_months, EXCLUDED.full_months)"
        , ";"
        ]

servePowerUsersReport :: Ctx -> IO PowerUserReport
servePowerUsersReport ctx = do
    cachedIO' ctx () $ runIntegrations' ctx powerUserReport

servePowerProjectsReport :: Ctx -> IO PowerProjectsReport
servePowerProjectsReport ctx = do
    cachedIO' ctx () $ runIntegrations' ctx powerProjectsReport

servePowerAbsencesReport :: Ctx -> Maybe Month -> IO PowerAbsenceReport
servePowerAbsencesReport ctx mmonth = do
    cachedIO' ctx mmonth $ runIntegrations' ctx $ powerAbsenceReport mmonth

serveTimereportsByTaskReport :: Ctx -> IO TimereportsByTaskReport
serveTimereportsByTaskReport ctx = cachedIO' ctx () $
    runIntegrations' ctx timereportsByTaskReport

cachedIO' :: (Eq k, Hashable k, Typeable k, NFData v, Typeable v) => Ctx -> k -> IO v -> IO v
cachedIO' ctx = cachedIO logger cache 600
  where
    cache  = ctxCache ctx
    logger = ctxLogger ctx

-- All report endpoints
-- this is used for api 'server' and pericron
reports :: NP ReportEndpoint Reports
reports =
    ReportEndpoint serveMissingHoursReport :*
    ReportEndpoint serveTimereportsByTaskReport :*
    Nil

{-
serveTable
    :: (Typeable key, KnownSymbol key, Typeable v, NFData v)
    => Integrations '[I, I, Proxy, I, I, I] v
    -> (v -> HtmlPage key)
    -> Ctx
    -> IO (HtmlPage key)
serveTable f g ctx = do
    v <- cachedIO' ctx () $ runIntegrations' ctx f
    pure (g v)
-}

serveData
    :: (Typeable v, NFData v)
    => Integrations '[I, I, Proxy, I, I, I] v
    -> Ctx
    -> IO v
serveData f ctx = cachedIO' ctx () $ runIntegrations' ctx f

serveChart
    :: (Typeable key, KnownSymbol key, Typeable v, NFData v)
    => Integrations '[I, I, Proxy, I, I, I] v
    -> (v -> Chart key)
    -> Ctx
    -> IO (Chart key)
serveChart f g ctx = do
    v <- cachedIO' ctx () $ runIntegrations' ctx f
    pure (g v)

serveChartIO
    :: (Typeable key, KnownSymbol key, Typeable v, NFData v)
    => LogT IO v
    -> (v -> Chart key)
    -> Ctx
    -> IO (Chart key)
serveChartIO f g ctx = do
    v <- cachedIO' ctx () $ runLogT "chart" (ctxLogger ctx) f
    pure (g v)

serveGraph
    :: (Typeable key, KnownSymbol key, Typeable a, NFData a)
    => Integrations '[I, I, Proxy, I, I, I] (Graph a key)
    -> Ctx
    -> IO (Graph a key)
serveGraph m ctx = cachedIO' ctx () $ runIntegrations' ctx m

missingHoursChartData'
    :: Ctx
    -> Integrations '[I, I, Proxy, I, I, I] MissingHoursChartData
missingHoursChartData' _ctx =
    missingHoursChartData missingHoursEmployeePredicate

makeServer
    :: All RClass reports
    => Ctx -> NP ReportEndpoint reports -> Server (FoldReportsAPI reports)
makeServer _   Nil       = pure indexPage
makeServer ctx (r :* rs) =
    let s = handler r
    in s :<|> s :<|> s :<|> makeServer ctx rs
  where
    handler :: forall r. RClass r => ReportEndpoint r -> Handler (RReport r)
    handler re@(ReportEndpoint re') = liftIO $ do
        mark $ "Report: " <> path re
        re' ctx

    path :: forall r. RClass r => ReportEndpoint r -> Text
    path _ = symbolVal (Proxy :: Proxy (RName r)) ^. packed

-- | API server
server :: Ctx -> Server ReportsAPI
server ctx = makeServer ctx reports
    -- tables
    :<|> liftIO (serveData activeAccountsData ctx)
    :<|> liftIO (serveData activeAccountsData ctx)
    -- charts
    :<|> liftIO (serveChart utzChartData utzChartRender ctx)
    :<|> liftIO (serveChart (missingHoursChartData' ctx) missingHoursChartRender ctx)
    :<|> liftIO (serveChartIO (missingHoursDailyChartData ctx) missingHoursDailyChartRender ctx)
    :<|> liftIO (serveChart careerLengthData careerLengthRender ctx)
    :<|> liftIO (serveChart careerLengthData careerLengthRelativeRender ctx)
    -- graphs
    :<|> liftIO (serveGraph supervisorsGraph ctx)
    -- power
    :<|> liftIO (servePowerUsersReport ctx)
    :<|> liftIO (servePowerProjectsReport ctx)
    :<|> liftIO . servePowerAbsencesReport ctx
    -- missing hours notifications
    :<|> liftIO (missingHoursNotifications ctx)
    -- dashdo
    :<|> ctxDashdo ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService        .~ ReportsService
    & serverDescription    .~ "Various reports"
    & serverColour         .~ (Proxy :: Proxy ('FutuAccent 'AF2 'AC3))
    & serverApp reportsApi .~ server
    & serverEnvPfx         .~ "REPORTSAPP"
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
    makeCtx cfg lgr manager cache mq = do
        pp <- createPostgresPool $ cfgPostgresConnInfo cfg

        let ctx' = (cache, manager, lgr, cfg)
        dashDoApp <- makeDashdoServer ctx'
        let ctx = Ctx cache manager lgr cfg dashDoApp pp

        let missingHoursStatsJob =
                mkJob "missing-hours-stats" (missingHoursStats ctx) $ every (60 * 60)

        let jobs = missingHoursStatsJob : hcollapse
                (hcmap (Proxy :: Proxy RClass) (K . mkReportPeriocron ctx) reports)

        -- listen to MQ, especially for missing hours ping
        void $ forEachMessage mq $ \msg -> case msg of
            MissingHoursPing -> void $ missingHoursNotifications ctx
            _                -> pure ()

        return (ctx, jobs)

    mkReportPeriocron :: forall r. RClass r => Ctx -> ReportEndpoint r -> Job
    mkReportPeriocron ctx (ReportEndpoint r) = mkJob (name ^. packed) (r ctx)
        $ shifted (2 * 60) $ every $ 10 * 60
      where
        name = "Updating report " <> symbolVal (Proxy :: Proxy (RName r))
