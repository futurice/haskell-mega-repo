{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Futurice.App.Reports.LongAbsence (longAbsencesNotification) where

import Data.Aeson                (object, (.=))
import Data.Time.Calendar        (addDays)
import Futurice.Integrations
import Futurice.Office           (offBerlin, offMunich, offStuttgart)
import Futurice.Prelude
import Numeric.Interval.NonEmpty ((...))
import Prelude ()

import Futurice.App.Reports.Config
import Futurice.App.Reports.Ctx
import Futurice.App.Reports.Templates

import qualified Data.HashMap.Strict            as HM
import qualified Data.Map                       as Map
import qualified Data.Set                       as S
import qualified Data.Vector                    as V
import qualified Futurice.App.EmailProxy.Client as E
import qualified Futurice.App.EmailProxy.Types  as E
import qualified Futurice.IdMap                 as IdMap
import qualified Personio                       as P
import qualified PlanMill                       as PM
import qualified PlanMill.Queries               as PMQ

-- Check all employees that have been on absence for extended period
returningEmployees :: (MonadPlanMillQuery m, MonadTime m, MonadPersonio m) => Integer -> m (Day, [P.Employee])
returningEmployees dayLookup = do
    now <- currentDay
    let startDay = addDays (-(180-dayLookup-1)) now
    let endDay = addDays (dayLookup - 1) now
    let checkInterval = startDay ... endDay
    -- make set of individual absence days that can be compared with capacity date set
    absences <- Map.fromListWith (<>)
                . concatMap (\absence -> [(PM.absencePerson absence, S.singleton date) | date <- [PM.absenceStart absence .. PM.absenceFinish absence]])
                . V.toList
                . V.filter (\absence -> PM.absenceFinish absence >= startDay)
                <$> PMQ.absences
    fpm <- personioPlanmillMap
    let planmillUserIdToPersonioEmployee = Map.fromList $ (\(_,(emp,planmillUser)) -> (planmillUser ^. PM.identifier, emp)) <$> HM.toList fpm
    let fpm' = HM.filter (\(p, _) -> P.employeeIsActiveWholeInterval checkInterval p && p ^. P.employeeEmploymentType == Just P.Internal) fpm
    capacitiesPerUser <- for (toList fpm') $ \(_, planmillUser) -> do
        capacities <- PMQ.capacities checkInterval (planmillUser ^. PM.identifier)
        caps <- for capacities $ \(PM.UserCapacity date amount _) -> do
            if amount > 0 then
              pure $ Just date
            else
              pure Nothing
        pure $ (planmillUser ^. PM.identifier, S.fromList $ catMaybes $ V.toList caps)
    let capacitiesPerUserMap = Map.fromListWith (<>) capacitiesPerUser
    let setDifference key val = maybe val (S.difference val) (Map.lookup key absences)
    let capacitiesNotCoveredByAbsences = filter (\(_, set) -> length set == 0) $ Map.toList $ Map.mapWithKey setDifference capacitiesPerUserMap
    let personsOnLongAbsence = map fst capacitiesNotCoveredByAbsences
    (addDays dayLookup now,) . catMaybes <$> traverse (isReturning now planmillUserIdToPersonioEmployee absences) personsOnLongAbsence
  where
    isReturning now pmap absences uid = do
        capacities <- PMQ.capacities (addDays dayLookup now ... addDays dayLookup now) uid
        let workday = S.fromList $ map (\(PM.UserCapacity date _ _) -> date) $ V.toList $ V.filter (\(PM.UserCapacity _ amount _) -> amount > 0) capacities
        let workdayInWork =
                case Map.lookup uid absences of
                    Just absencesForPerson -> S.difference workday absencesForPerson
                    Nothing -> workday
        if length workdayInWork > 0 then
            pure $ Map.lookup uid pmap
        else
            pure $ Nothing

longAbsencesNotification :: Ctx -> IO ()
longAbsencesNotification ctx = do
    employees <- runIntegrations' ctx $ P.personio P.PersonioEmployees
    let empMap = IdMap.fromFoldable employees
    let dayLookups = [7,14,30]
    returners <- liftIO $ traverse (runIntegrations' ctx . returningEmployees) dayLookups
--    print returners
    for_ returners $ \(day, rs) -> for_ rs $ \emp -> do
        let office = emp ^. P.employeeOffice
        if office == offBerlin || office == offMunich || office == offStuttgart then do
            let params = object
                  [ "supervisorName" .= (fromMaybe "No supervisor found" $ emp ^. P.employeeSupervisorId >>= \s -> empMap ^.at s >>= Just . (^. P.employeeFullname))
                  , "name"           .= (emp ^. P.employeeFullname)
                  , "returnDate"     .= day
                  ]
            x <- liftIO $ tryDeep $ E.sendEmail mgr emailProxyBurl $ E.emptyReq (E.fromEmail (cfgHcGermanyEmail cfg))
                 & E.reqSubject .~ "Returning employee"
                 & E.reqBody    .~ renderMustache returningEmployeeEmailGermanyTemplate params ^. strict
            case x of
              Left exc -> runLogT "returning-employee-notification" lgr $ logAttention "sendEmail failed" (show exc)
              Right () -> runLogT "returning-employee-notification" lgr $ logInfo "Returning employee email send with params " params
        else do
            case emp ^. P.employeeSupervisorId >>= \s -> empMap ^.at s of
              Just supervisor -> case supervisor ^. P.employeeEmail of
                Just supervisorEmail ->  do
                    let params = object
                           [ "supervisorName" .= (supervisor ^. P.employeeFirst)
                           , "name"           .= (emp ^. P.employeeFullname)
                           , "returnDate"     .= day
                           ]
                    x <- liftIO $ tryDeep $ E.sendEmail mgr emailProxyBurl $ E.emptyReq (E.fromEmail supervisorEmail)
                        & E.reqCc      .~ fmap (pure . E.fromEmail) (cfgHcEmailCC cfg)
                        & E.reqSubject .~ "Returning employee"
                        & E.reqBody    .~ renderMustache returningEmployeeEmailTemplate params ^. strict
                    case x of
                      Left exc -> runLogT "returning-employee-notification" lgr $ logAttention "sendEmail failed" (show exc)
                      Right () -> runLogT "returning-employee-notification" lgr $ logInfo "Returning employee email send with params " params
                Nothing -> runLogT "returning-employee-notification" lgr $ logAttention "No email for supervisor " (show $ supervisor ^. P.employeeFullname)
              Nothing -> runLogT "returning-employee-notification" lgr $ logAttention "No supervisor for employee " (show $ emp ^. P.employeeFullname)
    pure ()
  where
    mgr = ctxManager ctx
    cfg = ctxConfig ctx
    lgr = ctxLogger ctx

    emailProxyBurl  = cfgEmailProxyBaseurl cfg
