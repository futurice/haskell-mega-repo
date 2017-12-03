{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
#endif
module Futurice.App.Reports (defaultMain) where

import Control.Lens               (_5)
import Dashdo.Servant             (DashdoAPI)
import Futurice.Integrations
       (Integrations, beginningOfPrev2Month, runIntegrations)
import Futurice.Metrics.RateMeter (mark)
import Futurice.Periocron
import Futurice.Prelude
import Futurice.Servant
import Generics.SOP               (All, hcmap, hcollapse)
import GHC.TypeLits               (KnownSymbol, symbolVal)
import Network.HTTP.Client        (httpLbs, parseUrlThrow, responseBody)
import Numeric.Interval.NonEmpty  ((...))
import Prelude ()
import Servant
import Servant.Chart              (Chart (..))
import Servant.Graph              (Graph (..))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified GitHub               as GH

import Futurice.App.Reports.API
import Futurice.App.Reports.CareerLengthChart
       (careerLengthData, careerLengthRelativeRender, careerLengthRender)
import Futurice.App.Reports.Config
import Futurice.App.Reports.Dashdo            (makeDashdoServer)
import Futurice.App.Reports.FumFlowdock
       (FumFlowdockReport, fumFlowdockReport)
import Futurice.App.Reports.GithubIssues
       (GitHubRepo (..), IssueReport, issueReport)
import Futurice.App.Reports.GithubUsers
       (GithubUsersReport, githubUsersReport)
import Futurice.App.Reports.Markup
import Futurice.App.Reports.MissingHours
       (MissingHoursReport, missingHoursReport)
import Futurice.App.Reports.MissingHoursChart
       (MissingHoursChartData, missingHoursChartData, missingHoursChartRender)
import Futurice.App.Reports.PlanmillEmployees
       (PlanmillEmployeesReport, planmillEmployeesReport)
import Futurice.App.Reports.PowerAbsences
       (PowerAbsenceReport, powerAbsenceReport)
import Futurice.App.Reports.PowerProjects
       (PowerProjectsReport, powerProjectsReport)
import Futurice.App.Reports.PowerUser         (PowerUserReport, powerUserReport)
import Futurice.App.Reports.SupervisorsGraph  (supervisorsGraph)
import Futurice.App.Reports.TimereportsByTask
       (TimereportsByTaskReport, timereportsByTaskReport)
import Futurice.App.Reports.UtzChart          (utzChartData, utzChartRender)

-- /TODO/ Make proper type
type Ctx = (Cache, Manager, Logger, Config, Server DashdoAPI)

newtype ReportEndpoint r = ReportEndpoint (Ctx -> IO (RReport r))

ctxConfig :: Ctx -> Config
ctxConfig (_, _, _, cfg, _) = cfg

-------------------------------------------------------------------------------
-- Integrations
-------------------------------------------------------------------------------

runIntegrations' :: Ctx -> Integrations I I Proxy I I I a -> IO a
runIntegrations' (_, mgr, lgr, cfg, _) m = do
    now <- currentTime
    runIntegrations mgr lgr now (cfgIntegrationsCfg cfg) m

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

-- Note: we cachedIO with () :: () as a key. It's ok as 'Cache'
-- uses both @key@ and @value@ TypeRep's as key to non-typed map.


serveIssues :: Ctx -> IO IssueReport
serveIssues ctx@(_, mgr, _, cfg, _) = cachedIO' ctx () $ do
    repos' <- repos mgr (cfgReposUrl cfg)
    runIntegrations' ctx
        (issueReport repos')

serveGithubUsersReport :: Ctx -> IO GithubUsersReport
serveGithubUsersReport ctx = cachedIO' ctx () $
    runIntegrations' ctx githubUsersReport

serveFumFlowdockReport :: Ctx -> IO FumFlowdockReport
serveFumFlowdockReport ctx = cachedIO' ctx () $
    runIntegrations' ctx fumFlowdockReport

serveMissingHoursReport
    :: (KnownSymbol title, Typeable title)
    => Bool -> Ctx -> IO (MissingHoursReport title)
serveMissingHoursReport allContracts ctx = do
    cachedIO' ctx allContracts $ do
        day <- currentDay
        -- TODO: end date to the last friday
        let interval = beginningOfPrev2Month day ... pred day
        runIntegrations' ctx (missingHoursReport contractTypes interval)
  where
    contractTypes
        | allContracts = Nothing
        | otherwise    = Just (cfgMissingHoursContracts (ctxConfig ctx))

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

servePlanmillEmployeesReport :: Ctx -> IO PlanmillEmployeesReport
servePlanmillEmployeesReport ctx = cachedIO' ctx () $
    runIntegrations' ctx planmillEmployeesReport

cachedIO' :: (Eq k, Hashable k, Typeable k, NFData v, Typeable v) => Ctx -> k -> IO v -> IO v
cachedIO' (cache, _, logger, _, _) = cachedIO logger cache 600

-- All report endpoints
-- this is used for api 'server' and pericron
reports :: NP ReportEndpoint Reports
reports =
    ReportEndpoint serveIssues :*
    ReportEndpoint serveFumFlowdockReport :*
    ReportEndpoint serveGithubUsersReport :*
    ReportEndpoint (serveMissingHoursReport True) :*
    ReportEndpoint (serveMissingHoursReport False) :*
    ReportEndpoint serveTimereportsByTaskReport :*
    ReportEndpoint servePlanmillEmployeesReport :*
    Nil

serveChart
    :: (Typeable key, KnownSymbol key, Typeable v, NFData v)
    => Integrations I I Proxy I I I v
    -> (v -> Chart key)
    -> Ctx
    -> IO (Chart key)
serveChart f g ctx = do
    v <- cachedIO' ctx () $ runIntegrations' ctx f
    pure (g v)

serveGraph
    :: (Typeable key, KnownSymbol key, Typeable a, NFData a)
    => Integrations I I Proxy I I I (Graph a key)
    -> Ctx
    -> IO (Graph a key)
serveGraph m ctx = cachedIO' ctx () $ runIntegrations' ctx m

-- TODO: introduce "HasMissingHoursContracts"?
missingHoursChartData'
    :: Ctx
    -> Integrations I I Proxy I I I MissingHoursChartData
missingHoursChartData' ctx =
    missingHoursChartData (cfgMissingHoursContracts (ctxConfig ctx))

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
    -- dashdo
    :<|> view _5 ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName           .~ "Report API"
    & serverDescription    .~ "Various reports"
    & serverColour         .~ (Proxy :: Proxy ('FutuAccent 'AF2 'AC3))
    & serverApp reportsApi .~ server
    & serverEnvPfx         .~ "REPORTSAPP"
  where
    makeCtx :: Config -> Logger -> Cache -> IO (Ctx, [Job])
    makeCtx cfg lgr cache = do
        manager <- newManager tlsManagerSettings
        let ctx' = (cache, manager, lgr, cfg)
        dashDoApp <- makeDashdoServer ctx'
        let ctx = (cache, manager, lgr, cfg, dashDoApp)

        let jobs = hcollapse $
                hcmap (Proxy :: Proxy RClass) (K . mkReportPeriocron ctx) reports

        return (ctx, jobs)

    mkReportPeriocron :: forall r. RClass r => Ctx -> ReportEndpoint r -> Job
    mkReportPeriocron ctx (ReportEndpoint r) = mkJob (name ^. packed) (r ctx)
        $ shifted (2 * 60) $ every $ 10 * 60
      where
        name = "Updating report " <> symbolVal (Proxy :: Proxy (RName r))

-------------------------------------------------------------------------------
-- Temporary
-------------------------------------------------------------------------------

-- | We download the list
repos :: Manager -> Text -> IO [GitHubRepo]
repos mgr url = do
    req <- parseUrlThrow $ T.unpack url
    res <- decodeUtf8Lenient . LBS.toStrict . responseBody <$> httpLbs req mgr
    return $ mapMaybe f $ T.lines res
  where
    f line = case T.words line of
      [o, n] -> Just $ GitHubRepo (GH.mkOwnerName o) (GH.mkRepoName n)
      _      -> Nothing
