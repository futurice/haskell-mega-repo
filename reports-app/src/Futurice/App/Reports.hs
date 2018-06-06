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

import Control.Lens                (_4, _5)
import Dashdo.Servant              (DashdoAPI)
import Data.Aeson                  (object, (.=))
import Data.Time                   (addDays, defaultTimeLocale, formatTime)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Futurice.Integrations
       (Integrations, beginningOfPrev2Month, personioPlanmillMap,
       runIntegrations)
import Futurice.Metrics.RateMeter  (mark)
import Futurice.Periocron
import Futurice.Prelude
import Futurice.Report.Columns     (Report (..))
import Futurice.Servant
import Generics.SOP                (All, hcmap, hcollapse)
import GHC.TypeLits                (KnownSymbol, symbolVal)
import Numeric.Interval.NonEmpty   (Interval, (...))
import Prelude ()
import Servant
import Servant.Chart               (Chart (..))
import Servant.Graph               (Graph (..))

import Futurice.App.Preferences.Client (getPreferences)
import Futurice.App.Preferences.Types

import Futurice.App.Reports.API
import Futurice.App.Reports.CareerLengthChart
       (careerLengthData, careerLengthRelativeRender, careerLengthRender)
import Futurice.App.Reports.Config
import Futurice.App.Reports.Dashdo            (makeDashdoServer)
import Futurice.App.Reports.Markup
import Futurice.App.Reports.MissingHours
       (MissingHoursReport, missingHourDay, missingHoursReport)
import Futurice.App.Reports.MissingHoursChart
       (MissingHoursChartData, missingHoursChartData, missingHoursChartRender)
import Futurice.App.Reports.PowerAbsences
       (PowerAbsenceReport, powerAbsenceReport)
import Futurice.App.Reports.PowerProjects
       (PowerProjectsReport, powerProjectsReport)
import Futurice.App.Reports.PowerUser         (PowerUserReport, powerUserReport)
import Futurice.App.Reports.SupervisorsGraph  (supervisorsGraph)
import Futurice.App.Reports.Templates
import Futurice.App.Reports.TimereportsByTask
       (TimereportsByTaskReport, timereportsByTaskReport)
import Futurice.App.Reports.UtzChart          (utzChartData, utzChartRender)

import qualified Futurice.App.EmailProxy.Client as E
import qualified Futurice.App.EmailProxy.Types  as E
import qualified Futurice.App.SmsProxy.Client   as S
import qualified Futurice.App.SmsProxy.Types    as S

import qualified Data.HashMap.Strict as HM
import qualified Personio            as P

-- /TODO/ Make proper type
type Ctx = (Cache, Manager, Logger, Config, Server DashdoAPI)

newtype ReportEndpoint r = ReportEndpoint (Ctx -> IO (RReport r))

-------------------------------------------------------------------------------
-- Integrations
-------------------------------------------------------------------------------

runIntegrations' :: Ctx -> Integrations '[I, I, Proxy, I, I, I] a -> IO a
runIntegrations' (_, mgr, lgr, cfg, _) m = do
    now <- currentTime
    runIntegrations mgr lgr now (cfgIntegrationsCfg cfg) m

-------------------------------------------------------------------------------
-- Missing hours predicates
-------------------------------------------------------------------------------

-- Basic predicate, include all internal people
missingHoursEmployeePredicate :: Interval Day -> P.Employee -> Bool
missingHoursEmployeePredicate interval p = and
    [ p ^. P.employeeEmploymentType == Just P.Internal
    , P.employeeIsActiveInterval interval p
    ]

-- Stricter predicate
missingHoursEmployeePredicate' :: Interval Day -> P.Employee -> Bool
missingHoursEmployeePredicate' interval p = and
    [ p ^. P.employeeEmploymentType == Just P.Internal
    , p ^. P.employeeSalaryType == Just P.Monthly
    , P.employeeIsActiveInterval interval p
    ]

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

-- Note: we cachedIO with () :: () as a key. It's ok as 'Cache'
-- uses both @key@ and @value@ TypeRep's as key to non-typed map.

serveMissingHoursReport
    :: (KnownSymbol title, Typeable title)
    => Bool -> Ctx -> IO (MissingHoursReport title)
serveMissingHoursReport allContracts ctx = do
    cachedIO' ctx allContracts $ do
        day <- currentDay
        let interval = beginningOfPrev2Month day ... previousFriday day
        runIntegrations' ctx (missingHoursReport predicate interval)
  where
    predicate
        | allContracts = missingHoursEmployeePredicate
        | otherwise    = missingHoursEmployeePredicate'

previousFriday :: Day -> Day
previousFriday d
    | wd >= 6   = addDays (fromIntegral $ 7 - wd) d
    | otherwise = addDays (fromIntegral $ -2 - wd) d
  where
    (_, _, wd) = toWeekDate d

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
cachedIO' (cache, _, logger, _, _) = cachedIO logger cache 600

-- All report endpoints
-- this is used for api 'server' and pericron
reports :: NP ReportEndpoint Reports
reports =
    ReportEndpoint (serveMissingHoursReport True) :*
    ReportEndpoint (serveMissingHoursReport False) :*
    ReportEndpoint serveTimereportsByTaskReport :*
    Nil

serveChart
    :: (Typeable key, KnownSymbol key, Typeable v, NFData v)
    => Integrations '[I, I, Proxy, I, I, I] v
    -> (v -> Chart key)
    -> Ctx
    -> IO (Chart key)
serveChart f g ctx = do
    v <- cachedIO' ctx () $ runIntegrations' ctx f
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
    missingHoursChartData missingHoursEmployeePredicate'

missingHoursNotifications :: Ctx -> IO Text
missingHoursNotifications ctx = runLogT "missing-hours-notifications" lgr $ do
    day <- currentDay
    -- TODO: end date to the last friday
    let interval = beginningOfPrev2Month day ... previousFriday day

    (ppm, Report _ report) <- liftIO $ runIntegrations' ctx $ (,)
        <$>personioPlanmillMap
        <*> missingHoursReport missingHoursEmployeePredicate' interval

    let logins = HM.keys report
    prefs <- liftIO $ getPreferences mgr preferencesBurl logins
    ifor_ report $ \login r -> do
        let pref = fromMaybe defaultPreferences $ prefs ^? ix login

        case ppm ^? ix login . _1 of
            Nothing -> logAttention "Unknown login" login
            Just p -> do
                let humanDay :: Day -> Value
                    humanDay d = object
                        [ "day" .= formatTime defaultTimeLocale "%A %F" d
                        ]

                let days = r ^.. _2 . folded . missingHourDay . getter humanDay

                let params = object
                        [ "name"     .= (p ^. P.employeeFirst)
                        , "interval" .= show interval
                        , "ndays"    .= length days
                        , "hours"    .= days
                        ]

                when (pref ^. prefHoursPingEmail) $ case p ^. P.employeeEmail of
                    Nothing -> logAttention "Employee without email" login
                    Just addr -> do
                        x <- liftIO $ tryDeep $ E.sendEmail mgr emailProxyBurl $ E.emptyReq (E.fromEmail addr)
                            & E.reqSubject .~ "Missing hours"
                            & E.reqBody    .~ renderMustache missingHoursEmailTemplate params ^. strict
                        case x of
                            Left exc -> logAttention "sendEmail failed" (show exc)
                            Right () -> return ()

                when (pref ^. prefHoursPingSMS) $ case p ^. P.employeeWorkPhone of
                    Nothing -> logAttention "Employee without phone" login
                    Just numb -> do
                        x <- liftIO $ tryDeep $ S.sendSms mgr smsProxyBurl $ S.Req numb $
                            renderMustache missingHoursSmsTemplate params ^. strict
                        case x of
                            Left exc -> logAttention "sendSms failed" (show exc)
                            Right _  -> return ()


    return "OK"
  where
    mgr = ctx ^. _2
    lgr = ctx ^. _3
    cfg = ctx ^. _4

    preferencesBurl = cfgPreferencesAppBaseurl cfg
    emailProxyBurl  = cfgEmailProxyBaseurl cfg
    smsProxyBurl    = cfgSmsProxyBaseurl cfg

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
    -- charts
    :<|> liftIO (serveChart utzChartData utzChartRender ctx)
    :<|> liftIO (serveChart (missingHoursChartData' ctx) missingHoursChartRender ctx)
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
    :<|> view _5 ctx

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
        let ctx' = (cache, manager, lgr, cfg)
        dashDoApp <- makeDashdoServer ctx'
        let ctx = (cache, manager, lgr, cfg, dashDoApp)

        let jobs = hcollapse $
                hcmap (Proxy :: Proxy RClass) (K . mkReportPeriocron ctx) reports

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
