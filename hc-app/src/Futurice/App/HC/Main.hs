{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fconstraint-solver-iterations=0 #-}
module Futurice.App.HC.Main (defaultMain) where

import Data.Aeson                     (object, (.=))
import Data.Time                      (addDays, toGregorian)
import Futurice.App.EmailProxy.Client (sendEmail)
import Futurice.App.EmailProxy.Types
       (emptyReq, fromEmail, reqBody, reqCc, reqSubject)
import Futurice.FUM.MachineAPI        (FUM6 (..), fum6)
import Futurice.Integrations
       (Integrations, ServFUM6, ServPE, ServPM, beginningOfPrev2Month,
       personio, planmillEmployee, runIntegrations)
import Futurice.Lucid.Foundation
       (HtmlPage, a_, fullRow_, h1_, href_, p_, page_)
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant
import Numeric.Interval.NonEmpty      ((...))
import Prelude ()
import Servant
import Servant.Chart                  (Chart)
import Servant.Server.Generic
import System.Entropy                 (getEntropy)

import qualified Data.Map         as Map
import qualified Data.Set         as Set
import qualified Data.Vector      as V
import qualified FUM.Types.Login  as FUM
import qualified Futurice.IdMap   as IdMap
import qualified Personio         as P
import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ

import Futurice.App.HC.Achoo.Fetch
import Futurice.App.HC.Achoo.Render
import Futurice.App.HC.Achoo.Types        (AchooChart)
import Futurice.App.HC.Anniversaries
import Futurice.App.HC.API
import Futurice.App.HC.Config
import Futurice.App.HC.Ctx
import Futurice.App.HC.EarlyCaring.Page
import Futurice.App.HC.EarlyCaring.Types
import Futurice.App.HC.HRNumbers
import Futurice.App.HC.IndexPage
import Futurice.App.HC.PersonioValidation
import Futurice.App.HC.PrivateContacts
import Futurice.App.HC.VacationReport

server :: Ctx -> Server HCAPI
server ctx = genericServer $ Record
    { recIndex                = indexPageAction ctx
    , recPersonioValidations  = personioValidationAction ctx
    , recPrivateContacts      = personioPrivateContactsAction ctx
    , recAnniversaries        = anniversariesAction ctx
    , recHrNumbers            = hrnumbersAction ctx
    , recEarlyCaring          = earlyCaringAction ctx
    , recEarlyCaringCSV       = earlyCaringActionCSV ctx
    , recEarlyCaringSubmit    = earlyCaringSubmitAction ctx
    , recAchooReport          = achooReportAction ctx
    , recAchooChart           = achooChartAction ctx
    , recVacationReport       = vacationReportAction ctx
    , recVacationReportEmail  = vacationReportEmailAction ctx
    , recVacationReportSubmit = vacationReportSubmitAction ctx
    }

-------------------------------------------------------------------------------
-- Auth
-------------------------------------------------------------------------------

withAuthUser
    :: (MonadIO m, MonadTime m)
    => (FUM.Login -> Integrations '[ ServFUM6, ServPE, ServPM ] (HtmlPage a))
    -> Ctx -> Maybe FUM.Login
    -> m (HtmlPage a)
withAuthUser = withAuthUser' page404

withAuthUser'
    :: (MonadIO m, MonadTime m)
    => a
    -> (FUM.Login -> Integrations '[ ServFUM6, ServPE, ServPM ] a)
    -> Ctx -> Maybe FUM.Login
    -> m a
withAuthUser' def action ctx mfu = case mfu <|> cfgMockUser cfg of
    Nothing -> return def
    Just fu -> do
        now <- currentTime
        liftIO $ runIntegrations mgr lgr now (cfgIntegrationsCfg cfg) $ do
            fus <- mconcat <$> traverse (fum6 . FUMGroupEmployees) (cfgAccessGroups cfg)
            if fu `Set.notMember` fus
            then return def
            else action fu
  where
    cfg = ctxConfig ctx
    mgr = ctxManager ctx
    lgr = ctxLogger ctx

withAuthUser''
    :: (MonadIO m, MonadTime m)
    => a
    -> (FUM.Login -> Bool -> Integrations '[ ServFUM6, ServPE, ServPM ] a)
    -> Ctx -> Maybe FUM.Login
    -> m a
withAuthUser'' def action ctx mfu = case mfu <|> cfgMockUser cfg of
    Nothing -> return def
    Just fu -> do
        now <- currentTime
        liftIO $ runIntegrations mgr lgr now (cfgIntegrationsCfg cfg) $ do
            fus <- mconcat <$> traverse (fum6 . FUMGroupEmployees) (cfgAccessGroups cfg)
            action fu (fu `Set.member` fus)
  where
    cfg = ctxConfig ctx
    mgr = ctxManager ctx
    lgr = ctxLogger ctx

-------------------------------------------------------------------------------
-- Actions
-------------------------------------------------------------------------------

indexPageAction
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "index-page")
indexPageAction = withAuthUser (return . indexPage)

personioValidationAction
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "personio-validation")
personioValidationAction = withAuthUser $ \_ -> do
    today <- currentDay
    vs <- personio P.PersonioValidations
    return (validationReport vs today)

personioPrivateContactsAction
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "private-contacts")
personioPrivateContactsAction = withAuthUser $ \_ -> do
    es <- personio P.PersonioEmployees
    return (privateContacts es)

anniversariesAction
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "anniversaries")
anniversariesAction = withAuthUser $ \_ -> do
    today <- currentDay
    es <- personio P.PersonioEmployees
    return (anniversaries es today)

hrnumbersAction
    :: Ctx
    -> Maybe FUM.Login
    -> Handler (HtmlPage "hr-numbers")
hrnumbersAction = withAuthUser $ \_ -> do
    today <- currentDay
    es <- personio P.PersonioEmployees
    return (hrnumbers es today)

earlyCaringAction
    :: Ctx
    -> Maybe FUM.Login
    -> Bool
    -> Handler (HtmlPage "early-caring")
earlyCaringAction ctx mfu super = impl ctx mfu
  where
    impl = withAuthUser'' page404 $ \fu authed -> do
        today <- currentDay
        let interval = beginningOfPrev2Month today ... today
        earlyCaringPage (if authed && not super then Right secret else Left fu) today interval
            <$> personio P.PersonioEmployees
            <*> pmData interval
            <*> PMQ.absences
            <*> PMQ.allEnumerationValues Proxy Proxy

    secret = ctxSecret ctx

    pmData interval = do
        -- todo: make personio + planmill map function.
        us <- PMQ.users
        fmap catMaybes $ for (toList us) $ \u -> do
            let uid = u ^. PM.identifier
            for (PM.userLogin u) $ \login -> EarlyCaringPlanMill login uid
                <$> planmillEmployee uid
                <*> PMQ.capacities interval uid
                <*> PMQ.timereports interval uid
                <*> PMQ.userTimebalance uid

earlyCaringActionCSV
    :: Ctx
    -> Maybe FUM.Login
    -> Bool
    -> Handler [BalanceCSV]
earlyCaringActionCSV ctx mfu super = impl ctx mfu
  where
    impl = withAuthUser'' [] $ \fu authed -> do
        today <- currentDay
        let interval = beginningOfPrev2Month today ... today
        earlyCaringCSV (if authed && not super then Right secret else Left fu) (isPeopleManager fu) today
            <$> personio P.PersonioEmployees
            <*> pmData interval
            <*> PMQ.absences
            <*> PMQ.allEnumerationValues Proxy Proxy

    secret = ctxSecret ctx

    -- Will be replaced with more flexible auth
    isPeopleManager login = login == cfgPeopleManager (ctxConfig ctx)

    pmData interval = do
        -- todo: make personio + planmill map function.
        us <- PMQ.users
        fmap catMaybes $ for (toList us) $ \u -> do
            let uid = u ^. PM.identifier
            for (PM.userLogin u) $ \login -> EarlyCaringPlanMill login uid
                <$> planmillEmployee uid
                <*> PMQ.capacities interval uid
                <*> PMQ.timereports interval uid
                <*> PMQ.userTimebalance uid

achooReportAction
    :: Ctx
    -> Maybe FUM.Login
    -> Maybe Day
    -> Maybe Day
    -> Maybe Bool
    -> Handler (HtmlPage "achoo-report")
achooReportAction ctx mfu mi' ma' all' = withAuthUser (const impl) ctx mfu where
    impl = achooReportPage <$> achooReportFetch mi ma (fromMaybe True all')

    (mi, ma) = case (mi', ma') of
        (Just x,  Just y)  -> (x, y)
        (Nothing, Nothing) -> ( $(mkDay "2019-07-01"), $(mkDay "2019-12-31") )
        (Just x,  Nothing) -> (x, addDays 180 x) -- TODO: add 6 months
        (Nothing, Just x)  -> (x, addDays (negate 180) x)

achooChartAction
    :: Ctx
    -> Maybe FUM.Login
    -> AchooChart
    -> Day
    -> Day
    -> Bool
    -> Handler (Chart "achoo-chart")
achooChartAction ctx mfu ac mi ma a = withAuthUser' (error "404") impl ctx mfu where
    impl _ = achooRenderChart ac <$> achooReportFetch mi ma a

earlyCaringSubmitAction
    :: Ctx
    -> Maybe FUM.Login
    -> SignedBlob EarlyCaringEmail
    -> Handler Bool
earlyCaringSubmitAction ctx mfu sb = do
    x <- withAuthUser' False (const $ return True) ctx mfu
    if x then liftIO impl else return False
  where
    cfg = ctxConfig ctx
    mgr = ctxManager ctx
    lgr = ctxLogger ctx

    impl = runLogT "early-caring-submit" lgr $
        case verifySignedBlob (ctxSecret ctx) sb of
            Left err     -> do
                logAttention "Cannot verify data" err
                return False
            Right (EarlyCaringEmail toAddr subject body) -> do
                x <- liftIO $ tryDeep $ sendEmail mgr (cfgEmailProxyBaseurl cfg) req
                case x of
                    Left exc -> logAttention "sendEmail failed" (show exc) >> return False
                    Right () -> return True
              where
                req = emptyReq (fromEmail toAddr)
                    & reqSubject .~ subject
                    & reqBody    .~ body ^. strict
                    & reqCc      .~ fmap (pure . fromEmail) (cfgEarlyCaringCC cfg)

currentYear :: Day -> Integer
currentYear n =
    case toGregorian n of
      (year, _, _) -> year

futuriceGmbh :: Int
futuriceGmbh = 3426

-- need to be internal, non-inactive and not expats
employeesForVacationReport :: (P.MonadPersonio m) => m [P.Employee]
employeesForVacationReport = do
    employees <- personio P.PersonioEmployees
    pure $ filter (\e -> e ^. P.employeeExpat == False) $ filter (\e -> e ^. P.employeeStatus /= P.Inactive) $ filter (\e -> e ^. P.employeeEmploymentType == Just P.Internal) employees

vacationReportAction :: Ctx -> Maybe FUM.Login -> Handler (HtmlPage "vacation-report")
vacationReportAction ctx mfum = do
    logs <- liftIO $ runLogT "vacation-report" (ctxLogger ctx) $ safePoolQuery_ ctx "SELECT timestamp, receiver, sender FROM hc.vacationreportlog"
    let logMap = Map.fromListWith (\v1 v2 -> if (vrSendTime v1) > (vrSendTime v2) then v1 else v2) $ map (\l -> (vrReceiver l, l)) logs
    withAuthUser
        (\_ -> do
              vacations <- PMQ.earnedVacationsReport futuriceGmbh
              day <- currentDay
              let vacations' = V.filter
                      (\v -> PM._vacationYear v == Just (fromInteger $ currentYear day) || PM._vacationYear v == Just ((fromInteger $ currentYear day) - 1))
                      vacations
              employees' <- personio P.PersonioEmployees
              pure $ renderReport logMap vacations' employees') ctx mfum

vacationReportEmailAction :: Ctx -> Maybe FUM.Login -> P.EmployeeId -> Handler (HtmlPage "vacation-report-single")
vacationReportEmailAction ctx mfum eid = withAuthUser (\_ -> do
    d <- PMQ.earnedVacationsReport futuriceGmbh
    employees <- employeesForVacationReport
    let employees' = Map.fromList $ map (\e -> (e ^. P.employeeId, e)) employees
    case employees' ^.at eid of
      Nothing -> error ""
      Just employee -> do
          now <- currentDay
          pure $ renderReportSingle employee (currentYear now) d) ctx mfum

vacationReportSubmitAction :: Ctx -> Maybe FUM.Login -> Handler (CommandResponse ())
vacationReportSubmitAction ctx mfu = do
    x <- withAuthUser' Nothing (return . Just) ctx mfu
    case x of
      Just fum -> liftIO $ f fum
      Nothing -> return (CommandResponseError "Unauthorized")
  where
    lgr = ctxLogger ctx
    cfg = ctxConfig ctx
    mgr = ctxManager ctx
    supervisorEmail empMap e = do
        sid <- e ^. P.employeeSupervisorId
        supervisor <- empMap ^.at sid
        email <- supervisor ^. P.employeeEmail
        pure $ pure $ fromEmail email
    impl empMap fum curYear reports e = runLogT "vacation-report-submit" lgr $ do
        let body = reportSingle e curYear reports
        case (e ^. P.employeeEmail, body) of
          (Just email, Just body') -> do
            x <- liftIO $ tryDeep $ sendEmail mgr (cfgEmailProxyBaseurl cfg) $ emptyReq (fromEmail email)
                 & reqSubject .~ "Vacation report"
                 & reqBody    .~ body' ^. strict
                 & reqCc      .~ supervisorEmail empMap e
            case x of
              Left exc -> logAttention "sendEmail failed" (show exc) >> return Nothing
              Right () -> do
                  void $ safePoolExecute ctx "INSERT INTO hc.vacationreportlog (receiver, sender) VALUES (?,?)" $ ((e ^. P.employeeId), fum)
                  return (Just e)
          _ -> return Nothing
    f fum = do
        now <- currentTime
        day <- currentDay
        (reports,employees',empMap) <- liftIO $ runIntegrations mgr lgr now (cfgIntegrationsCfg cfg) $ do
            d <- PMQ.earnedVacationsReport futuriceGmbh
            employees' <- employeesForVacationReport
            allEmployees <- personio P.PersonioEmployees
            pure (d, employees', IdMap.fromFoldable allEmployees)
        sendResult <- for employees' $ impl empMap fum (currentYear day) reports
        liftIO $ runLogT "vacation-report-submit" lgr $ logInfo "Send vacation report" $ object
            [ "successfully" .= (length $ filter (/= Nothing) sendResult)
            ]
        pure CommandResponseReload

page404 :: HtmlPage a
page404 = page_ "HC - Unauthorised" $
    fullRow_ $ do
        h1_ "Unauthorised"
        p_ $ do
            "Ask IT Team for access rights. "
            "Or try " <> a_ [ href_ "/early-caring" ] "/early-caring"

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverService     .~ HCService
    & serverDescription .~ "Tools for HC"
    & serverApp hcApi   .~ server
    & serverColour      .~ (Proxy :: Proxy ('FutuAccent 'AF3 'AC2))
    & serverEnvPfx      .~ "HCAPP"
  where
    makeCtx :: () -> Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
    makeCtx () cfg lgr mgr _cache _mq = do
        secret <- getEntropy 64
        pp <- createPostgresPool $ cfgPostgresConnInfo cfg
        let ctx = Ctx cfg lgr mgr secret pp
        pure (ctx, [])
