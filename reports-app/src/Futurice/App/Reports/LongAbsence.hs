{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Reports.LongAbsence (longAbsencesNotification) where

import Data.Time.Calendar        (addDays)
import Futurice.Integrations
import Futurice.Prelude
import Numeric.Interval.NonEmpty ((...))
import Prelude ()

import Futurice.App.Reports.Ctx

import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as Map
import qualified Data.Set            as S
import qualified Data.Vector         as V
import qualified Personio            as P
import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ

-- Check all employees that have been on absence for extended period
longAbsences :: (MonadPlanMillQuery m, MonadTime m, MonadPersonio m) => Integer -> m [P.Employee]
longAbsences dayLookup = do
    now <- currentDay
--    let now = $(mkDay "2020-01-25")
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
    catMaybes <$> traverse (isReturning planmillUserIdToPersonioEmployee absences) personsOnLongAbsence
  where
    isReturning pmap absences uid = do
        now <- currentDay
--        let now = $(mkDay "2020-01-25")
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
    let dayLookups = [7,14,30]
    returners <- liftIO $ traverse (runIntegrations' ctx . longAbsences) dayLookups
    print returners
    pure ()
