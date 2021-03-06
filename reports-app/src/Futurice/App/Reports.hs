{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 -fprint-potential-instances #-}
module Futurice.App.Reports (defaultMain) where

import Control.Lens                   (each)
import Data.Aeson                     (object, (.=))
import Data.List                      (isSuffixOf)
import Futuqu                         (futuquServer)
import Futurice.App.OktaProxy.Client  (groupMembers)
import Futurice.Integrations
       (Integrations, beginningOfPrev2Month, endOfPrevMonth, previousFriday)
import Futurice.Periocron
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Report.Columns        (reportParams)
import Futurice.Servant
import Futurice.Time                  (unNDT)
import Futurice.Time.Month            (Month (..), dayToMonth, monthInterval)
import Futurice.Tribe
import Futurice.Wai.ContentMiddleware
import Numeric.Interval.NonEmpty      ((...))
import Peakon
       (engagementDrivers, engagementOverview, segments)
import Prelude ()
import Servant
import Servant.Cached                 (mkCached)
import Servant.Chart                  (Chart (..))
import Servant.Graph                  (Graph (..))
import Servant.Server.Generic         (genericServer)

import qualified Data.Set               as Set
import qualified Data.Swagger           as Sw
import qualified FUM.Types.Login        as FUM
import qualified Futurice.KleeneSwagger as K

import Futurice.App.Reports.API
import Futurice.App.Reports.ActiveAccounts
import Futurice.App.Reports.ActiveSubcontractorsByHours
       (ActiveSubcontractorData, activeSubcontractorsReport)
import Futurice.App.Reports.Capacity                          (hasCapacity)
import Futurice.App.Reports.CareerLengthChart
       (careerLengthData, careerLengthRelativeRender, careerLengthRender)
import Futurice.App.Reports.Config
import Futurice.App.Reports.Ctx
import Futurice.App.Reports.Dashdo                            (makeDashdoServer)
import Futurice.App.Reports.DoWeStudy                         (doWeStudyData)
import Futurice.App.Reports.FumAbsences                       (fumAbsences)
import Futurice.App.Reports.IDontKnow                         (iDontKnowData)
import Futurice.App.Reports.Inventory
import Futurice.App.Reports.Invoice                           (invoiceData)
import Futurice.App.Reports.LongAbsence
       (longAbsencesNotification)
import Futurice.App.Reports.Markup
import Futurice.App.Reports.MassSend                          (sendMessageToAll)
import Futurice.App.Reports.MissingHours
       (MissingHoursReport, MissingHoursSimplifiedReport, mhpTotalHours,
       missingHoursEmployeePredicate, missingHoursReport,
       missingHoursSimplifiedReport)
import Futurice.App.Reports.MissingHoursByProject
       (MissingHoursByProject, missingHoursByProject)
import Futurice.App.Reports.MissingHoursChart
       (MissingHoursChartData, missingHoursChartData, missingHoursChartRender)
import Futurice.App.Reports.MissingHoursDailyChart
       (missingHoursDailyChartData, missingHoursDailyChartRender)
import Futurice.App.Reports.MissingHoursNotifications
import Futurice.App.Reports.OKRCompetencies                   (competencyData)
import Futurice.App.Reports.OwnedComputers                    (userComputers)
import Futurice.App.Reports.PlanMillAccountValidation
       (pmAccountValidationData)
import Futurice.App.Reports.PowerAbsences
       (PowerAbsenceReport, powerAbsenceReport)
import Futurice.App.Reports.PowerAllRevenues
       (PowerAllRevenues, powerAllRevenuesReport)
import Futurice.App.Reports.PowerProjects
       (PowerProjectsReport, powerProjectsReport)
import Futurice.App.Reports.PowerUTZ
       (PowerUTZReport, powerUTZReport)
import Futurice.App.Reports.PowerUser
       (PowerUserReport, powerUserReport)
import Futurice.App.Reports.ProjectHours                      (projectHoursData)
import Futurice.App.Reports.ProjectMembers
       (projectMemberData)
import Futurice.App.Reports.PublicCareerLevel
       (publicCareerLevelData)
import Futurice.App.Reports.SubcontractorBillingNotifications
import Futurice.App.Reports.SubcontractorHoursNotifications
       (subcontractorHoursNotifications)
import Futurice.App.Reports.SupervisorsGraph                  (supervisorsGraph)
import Futurice.App.Reports.TeamsHoursByCategoryReport
       (TeamsHoursByCategoryReport, teamsHoursByCategoryReport)
import Futurice.App.Reports.TimereportsByTask
       (TimereportsByTaskReport, timereportsByTaskReport)
import Futurice.App.Reports.TimereportsDump                   (timereportsDump)
import Futurice.App.Reports.UtzChart
       (utzChartData, utzChartRender)
import Futurice.App.Reports.ValueCreation
       (ValueCreationReport, valueCreationReport)

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

serveMissingHoursSimplifiedReport :: Ctx -> Maybe Text -> Maybe Month -> Maybe Tribe -> IO MissingHoursSimplifiedReport
serveMissingHoursSimplifiedReport ctx memp mmonth mtribe = cachedIO' ctx (memp, mmonth, mtribe) $ do
    day <- currentDay
    let memp' =
            case memp of
              Just "-" -> Nothing
              e -> e
    let maxCurrentMonth m =
            let curMonth = dayToMonth day
            in if curMonth == m  then
                 min day (lastDayOfMonth m)
               else
                 lastDayOfMonth m
    let filterInterval = fmap (\m -> firstDayOfMonth m ... previousFriday (maxCurrentMonth m)) mmonth
    let interval = fromMaybe (beginningOfPrev2Month day ... previousFriday day) filterInterval
    runIntegrations' ctx (missingHoursSimplifiedReport missingHoursEmployeePredicate interval (beginningOfPrev2Month day ... previousFriday day) memp' mmonth mtribe)

serveMissingHoursByProjectReport :: Ctx -> Maybe Month -> Maybe Text -> IO MissingHoursByProject
serveMissingHoursByProjectReport ctx mmonth mtribe = cachedIO' ctx (mmonth, mtribe) $ do
    day <- currentDay
    let maxCurrentMonth m =
            let curMonth = dayToMonth day
            in if curMonth == m  then
                 min day (lastDayOfMonth m)
               else
                 lastDayOfMonth m
    let filterInterval = fmap (\m -> firstDayOfMonth m ... previousFriday (maxCurrentMonth m)) mmonth
    let interval = fromMaybe (beginningOfPrev2Month day ... previousFriday day) filterInterval
    runIntegrations' ctx (missingHoursByProject missingHoursEmployeePredicate interval (beginningOfPrev2Month day ... previousFriday day) mmonth mtribe)

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
cachedIO' ctx k action = cachedIO logger cache 600 k $ do
    (ts, x) <- clocked action
    runLogT "cached-io" logger $ logTraceI "Execution time $ts" $ object
        [ "ts" .= timeSpecToSecondsD ts
        ]
    return x
  where
    cache  = ctxCache ctx
    logger = ctxLogger ctx

{-
serveTable
    :: (Typeable key, KnownSymbol key, Typeable v, NFData v)
    => Integrations ReportIntegrations v
    -> (v -> HtmlPage key)
    -> Ctx
    -> IO (HtmlPage key)
serveTable f g ctx = do
    v <- cachedIO' ctx () $ runIntegrations' ctx f
    pure (g v)
-}

serveData
    :: (Typeable v, NFData v)
    => Integrations ReportIntegrations v
    -> Ctx
    -> IO v
serveData f = serveData' () (const f) id

serveDataParam
    :: (Typeable v, NFData v, Eq k, Hashable k, Typeable k)
    => k
    -> (k -> Integrations ReportIntegrations v)
    -> Ctx
    -> IO v
serveDataParam k f = serveData' k f id

serveDataParam2
    :: (Typeable v, NFData v, Eq k, Hashable k, Typeable k, Eq k', Hashable k', Typeable k')
    => k
    -> k'
    -> (k -> k' -> Integrations ReportIntegrations v)
    -> Ctx
    -> IO v
serveDataParam2 k k' f = serveData' (k, k') (uncurry f) id

serveDataParam3
    :: (Typeable v, NFData v, Eq k, Hashable k, Typeable k, Eq k', Hashable k', Typeable k', Eq k'', Hashable k'', Typeable k'')
    => k
    -> k'
    -> k''
    -> (k -> k' -> k'' -> Integrations ReportIntegrations v)
    -> Ctx
    -> IO v
serveDataParam3 k k' k'' f = serveData' (k, k', k'') (\(x,y,z) -> f x y z) id

serveData'
    :: (Typeable a, NFData a, Eq k, Hashable k, Typeable k)
    => k
    -> (k -> Integrations ReportIntegrations a)  -- ^ single cache
    -> (a -> b)                                       -- ^ multiple outputs?
    -> Ctx
    -> IO b
serveData' k f g ctx = do
    a <- cachedIO' ctx k $ runIntegrations' ctx (f k)
    return (g a)

serveChart
    :: (Typeable key, KnownSymbol key, Typeable v, NFData v)
    => Integrations ReportIntegrations v
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
    => Integrations ReportIntegrations (Graph a key)
    -> Ctx
    -> IO (Graph a key)
serveGraph m ctx = cachedIO' ctx () $ runIntegrations' ctx m

missingHoursChartData'
    :: Ctx
    -> Integrations ReportIntegrations MissingHoursChartData
missingHoursChartData' _ctx =
    missingHoursChartData missingHoursEmployeePredicate

serveActiveSubcontractorReport :: Ctx -> Maybe Day -> IO ActiveSubcontractorData
serveActiveSubcontractorReport ctx (Just d) = serveDataParam d activeSubcontractorsReport ctx
serveActiveSubcontractorReport ctx Nothing = do
    now <- currentDay
    serveDataParam now activeSubcontractorsReport ctx

serveAllRevenues2Report :: Ctx -> Maybe Month -> IO PowerAllRevenues
serveAllRevenues2Report ctx mmonth =
    cachedIO' ctx mmonth $ runIntegrations' ctx $ powerAllRevenuesReport mmonth

servePowerUTZReport :: Ctx -> Maybe Month -> IO PowerUTZReport
servePowerUTZReport ctx mmonth =
    cachedIO' ctx mmonth $ runIntegrations' ctx $ powerUTZReport mmonth

serveValueCreationReport :: Ctx -> Maybe Integer -> IO ValueCreationReport
serveValueCreationReport ctx myear =
    cachedIO' ctx myear $ runIntegrations' ctx $ valueCreationReport myear

serveTeamsHoursByCategoryReport :: Ctx -> Maybe Day -> Maybe Day -> IO TeamsHoursByCategoryReport
serveTeamsHoursByCategoryReport ctx startDay endDay = do
    monthDays <- monthInterval <$> currentMonth
    let startDay' = fromMaybe (minimum monthDays) startDay
    let endDay'   = fromMaybe (maximum monthDays) endDay
    runIntegrations' ctx $ teamsHoursByCategoryReport (startDay' ... endDay')

servePeakonEngagementOverviewData :: Ctx -> IO Value
servePeakonEngagementOverviewData ctx = runIntegrations' ctx $ engagementOverview

servePeakonEngagementDrivers :: Ctx -> IO Value
servePeakonEngagementDrivers ctx = runIntegrations' ctx $ engagementDrivers

servePeakonSegments :: Ctx -> IO Value
servePeakonSegments ctx = runIntegrations' ctx $ segments

serveSendMessageToAll :: Ctx -> Maybe FUM.Login -> Text -> IO ()
serveSendMessageToAll ctx mfum smstext =
    case mfum <|> cfgMockUser cfg of
      Nothing -> error "No user found"
      Just login -> do
          res <- groupMembers (ctxManager ctx) (cfgOktaProxyBaseurl cfg) (cfgITTeamOktaGroup cfg)
          if login `Set.member` Set.fromList res then
            sendMessageToAll ctx smstext
          else
            error "Not authorized"
  where
    cfg = ctxConfig ctx

-- | API server
server :: Ctx -> Server ReportsAPI
server ctx = genericServer $ Record
    { recIndex = liftIO $ indexPage <$> currentDay

    -- "legacy" -reports
    , recMissingHours = liftIO $ serveMissingHoursReport ctx
    , recHoursByTask  = liftIO $ serveTimereportsByTaskReport ctx
    , recMissingHoursSimplified =
      \emp month tribe -> liftIO $ serveMissingHoursSimplifiedReport
                          ctx
                          (emp >>= either (const Nothing) Just)
                          (month >>= either (const Nothing) Just)
                          (tribe >>= either (const Nothing) Just)
    , recMissingHoursByProject =
      \month tribe ->  liftIO $ serveMissingHoursByProjectReport
                          ctx
                          (month >>= either (const Nothing) Just)
                          (tribe >>= either (const Nothing) Just)
    -- Tables
    , recTablesActiveAccounts       = liftIO $ serveData activeAccountsData ctx
    , recTablesActiveAccountsJSON   = liftIO $ serveData activeAccountsData ctx
    , recTablesPMAccountValidation  = liftIO $ serveData pmAccountValidationData ctx
    , recTablesInventorySummary     = liftIO serveInventory
    , recTablesProjectHoursData     = liftIO $ serveData projectHoursData ctx
    , recTablesProjectHoursDataJSON = liftIO $ serveData projectHoursData ctx
    , recTablesIDontKnow            = \month tribe -> liftIO $ serveDataParam2 month (tribe >>= either (const Nothing) Just) iDontKnowData ctx
    , recTablesDoWeStudy            = \skind month tribe -> liftIO $ serveDataParam3 (skind >>= either (const Nothing) Just) month (tribe >>= either (const Nothing) Just) doWeStudyData ctx
    , recActiveSubcontractors       = liftIO . serveActiveSubcontractorReport ctx
    , recOKRCompetences             = liftIO $ serveData competencyData ctx
    , recOKRCompetencesJSON         = liftIO $ serveData competencyData ctx
    , recPublicCareerLevels         = liftIO $ serveData publicCareerLevelData ctx

    -- Dump: can be deleted, we have futuqu
    , recTimereportsDump = liftIO (serveData (mkCached <$> timereportsDump) ctx)

    -- Charts
    , recChartsUtz                  = liftIO $ serveChart utzChartData utzChartRender ctx
    , recChartsMissingHours         = liftIO $ serveChart (missingHoursChartData' ctx) missingHoursChartRender ctx
    , recChartsMissingHoursDaily    = liftIO $ serveChartIO (missingHoursDailyChartData ctx) missingHoursDailyChartRender ctx
    , recChartsCareerLength         = liftIO $ serveChart careerLengthData careerLengthRender ctx
    , recChartsCareerLengthRelative = liftIO $ serveChart careerLengthData careerLengthRelativeRender ctx
    , recChartsInventoryQuantiles   = liftIO $ serveInventoryChart inventoryBalanceQuantiles

    -- Graphs
    , recGraphsSupervisors = liftIO (serveGraph supervisorsGraph ctx)

    -- Additional non-reports
    , recPowerUsers    = liftIO $ servePowerUsersReport ctx
    , recPowerProjects = liftIO $ servePowerProjectsReport ctx
    , recPowerAbsences = liftIO . servePowerAbsencesReport ctx
    , recPowerAllRevenueReport = liftIO . serveAllRevenues2Report ctx
    , recPowerUTZ      = liftIO . servePowerUTZReport ctx

    -- For bubbleburster
    , recProjectMembers = liftIO $ serveData projectMemberData ctx

    -- For Data lake
    , recValueCreation = liftIO . serveValueCreationReport ctx

    , recTeamsHoursByCategory = \start end -> liftIO $ serveTeamsHoursByCategoryReport ctx start end

    -- missing hours notification
--    , recCommandMissingHoursNotification = liftIO $ missingHoursNotifications ctx

    -- futuqu
    , recFutuqu = futuquServer lgr (ctxManager ctx) (ctxCache ctx) (toFutuquCfg (cfgIntegrationsCfg cfg))

    -- dashdo
    , recDashdo =  ctxDashdo ctx
    , recComputers = liftIO serveComputerOwned

    -- peakon
    , recPeakonEngagementOverview = liftIO $ servePeakonEngagementOverviewData ctx
    , recPeakonEngagementDrivers = liftIO $ servePeakonEngagementDrivers ctx
    , recPeakonSegments         = liftIO $ servePeakonSegments ctx

    -- For futulog
    , recFumCapacity = \login month -> liftIO $ serveDataParam2 login month hasCapacity ctx
    , recFumAbsence = \login month -> liftIO $ serveDataParam2 login month fumAbsences ctx

    , recInvoice = \month -> liftIO $ serveDataParam month invoiceData ctx
    , recMassSendSMS = \user smstext -> liftIO $ serveSendMessageToAll ctx user smstext
    }
  where
    lgr = ctxLogger ctx
    cfg = ctxConfig ctx

    serveInventory = do
        pp <- createPostgresPool $ cfgPostgresConnInfoInv cfg
        xs <- runLogT "inventory-data" lgr $ inventorySummaryQuery pp

        serveData (inventorySummaryData xs) ctx

    serveInventoryChart g = do
        v <- serveInventory
        pure (g v)

    serveComputerOwned = do
        pp <- createPostgresPool $ cfgPostgresConnInfoInv cfg
        xs <- runLogT "inventory-data" lgr $ userComputers pp
        pure xs

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService        .~ ReportsService
    & serverDescription    .~ "Various reports"
    & serverColour         .~ (Proxy :: Proxy ('FutuAccent 'AF2 'AC3))
    & serverApp reportsApi .~ server
    & serverEnvPfx         .~ "REPORTSAPP"
    & serverMiddleware     .~ const contentMiddleware
    & serverSwaggerMod     .~ swaggerMod
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
    makeCtx cfg lgr manager cache mq = do
        pp <- createPostgresPool $ cfgPostgresConnInfo cfg

        let ctx' = (cache, manager, lgr, cfg)
        dashDoApp <- makeDashdoServer ctx'
        let ctx = Ctx cache manager lgr cfg dashDoApp pp

        let missingHoursStatsJob =
                mkJob "missing-hours-stats" (missingHoursStats ctx) $ every (60 * 60)

        let jobs = missingHoursStatsJob
                 : []

        -- listen to MQ, especially for missing hours ping
        void $ forEachMessage mq $ \msg -> case msg of
            MissingHoursPing       -> void $ missingHoursNotifications ctx
            SubcontractorPing      -> void $ subcontractorNotifications ctx
            SubcontractorHoursPing -> void $ subcontractorHoursNotifications ctx
            ReturningEmployeePing  -> void $ longAbsencesNotification ctx
            _                      -> pure ()

        return (ctx, jobs)

    swaggerMod
          = Sw.applyTagsFor
                (K.operationsMatching $ K.sym "power" *> many K.anySym)
                [ "For Power" ]
          . Sw.applyTagsFor
                (K.operationsMatching $ K.sym "charts" *> many K.anySym)
                [ "Charts" ]
          . Sw.applyTagsFor
                (K.operationsMatching $ K.sym "graphs" *> many K.anySym)
                [ "Graphs" ]
          . Sw.applyTagsFor
                (K.operationsMatching $ K.sym "dump" *> many K.anySym)
                [ "Data dumps" ]
          . Sw.applyTagsFor
                (K.operationsMatching $ K.sym "missing-hours" *> many K.anySym)
                [ "Missing hours" ]
          . Sw.applyTagsFor
                (K.operationsMatching $ K.sym "futuqu" *> many K.anySym)
                [ "FUTUrice QUeries: RAw DAta and aGGRRegates" ]
          . Sw.applyTagsFor
                (K.operationsMatching $ K.sym "tables" *> many K.anySym)
                [ "Tables" ]
          . Sw.applyTagsFor
                (K.operationsMatching $ K.sym "officevibe" *> many K.anySym)
                [ "For Officevibe" ]
          . Sw.applyTagsFor
                (K.operationsMatching $ many K.anySym *> K.psym (isSuffixOf "csv"))
                [ "CSV" ]
          . Sw.applyTagsFor
                (K.operationsMatching $ many K.anySym *> K.psym (isSuffixOf "json"))
                [ "JSON" ]
