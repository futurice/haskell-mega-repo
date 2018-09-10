{-# LANGUAGE DataKinds #-}
module Futurice.App.HC.Achoo.Fetch (achooReportFetch) where

import Control.Lens              (Getting)
import Data.Vec.Lazy.Inline      (Vec (..))
import FUM.Types.Login           (Login)
import Futurice.Integrations
import Futurice.Monoid
import Futurice.Prelude
import Numeric.Interval.NonEmpty (Interval, inf, sup, (...))
import Prelude ()

import Futurice.App.HC.Achoo.Types

import qualified Data.HashMap.Strict       as HM
import qualified Data.Map.Strict           as Map
import qualified Data.Set                  as Set
import qualified Numeric.Interval.NonEmpty as I
import qualified Personio                  as P
import qualified PlanMill                  as PM
import qualified PlanMill.Queries          as PMQ

achooReportFetch
    :: (MonadPersonio m, MonadPlanMillQuery m)
    => Day  -- ^ from
    -> Day  -- ^ to
    -> Bool -- ^ whole
    -> m AchooReport
achooReportFetch dayMin dayMax whole = do
    let interval = dayMin ... dayMax

    (fpm, absences, absenceTypes) <- (,,)
            <$> personioPlanmillMap
            <*> PMQ.absences
            <*> PMQ.allEnumerationValues Proxy Proxy

    let pmMap :: Map PM.UserId (Login, P.Employee)
        pmMap = Map.fromList
            [ (pmu ^. PM.identifier, (login, p))
            | (login, (p, pmu)) <- HM.toList fpm
            , p ^. P.employeeEmploymentType == Just P.Internal
            , if whole
              then P.employeeIsActiveWholeInterval interval p
              else P.employeeIsActiveInterval interval p
            ]

    let _absenceTypes :: Map (PM.EnumValue PM.Absence "absenceType") Text
        _absenceTypes = absenceTypes

    let absenceMapAll :: Map PM.UserId [Interval Day]
        absenceMapAll = Map.fromListWith (++) $ mapMaybe f (toList absences) where
            f a | PM.absenceAbsenceType a `notElem` sickAbsences = Nothing
                -- we aren't interested in older absences, let's filter them
                | PM.absenceFinish a < dayMin                    = Nothing
                | otherwise = Just (PM.absencePerson a, [PM.absenceStart a ... PM.absenceFinish a])

            -- NOTE: hardcoded values, as there is really no way to find out
            -- which types are sick leaves.
            sickAbsences = map PM.EnumValue [1010, 1025]

    -- absence map with trimmed intervals
    let absenceMapI   = mapMaybe (I.intersection interval) <$> absenceMapAll

    absenceMap <- ifor absenceMapI $ \pmUid as -> do
        caps <- Map.fromList . map (\uc -> (PM.userCapacityDate uc, PM.userCapacityAmount uc)) . toList
            <$> PMQ.capacities interval pmUid

        let counts :: Day -> Bool
            counts d = maybe True (> 0) $ caps ^? ix d

        let days' :: Set Day
            days' = view (folded . getter (\i -> Set.fromList [ inf i .. sup i])) as

        -- filter out days which don't have non-zero capacity
        let days = Set.filter counts days'

        return $ Duration $ length days

    let distrTribe = distrPer P.employeeTribe pmMap absenceMap
    let distrOffice = distrPer P.employeeOffice pmMap absenceMap
    let distrCountry = distrPer P.employeeCountry pmMap absenceMap

    return $ AchooReport
        { arInterval   = interval
        , arWhole      = whole

        , arPercentsTribe   = fmap toPercentages distrTribe
        , arPercentsOffice  = fmap toPercentages distrOffice
        , arPercentsCountry = fmap toPercentages distrCountry

        , arAverageTribe   = fmap toAverage distrTribe
        , arAverageOffice  = fmap toAverage distrOffice
        , arAverageCountry = fmap toAverage distrCountry

        , arDistrTribe   = distrTribe
        , arDistrOffice  = distrOffice
        , arDistrCountry = distrCountry
        }
  where
    toPercentages :: Map Duration Int -> PerSickDays Int
    toPercentages = ifoldMap absencesToPerSickDays

    toAverage :: Map Duration Int -> Double
    toAverage = getAverage . ifoldMap (\(Duration d) n ->
        Average (fromIntegral n) (fromIntegral d))

    distrPer
        :: Ord k
        => Getting k P.Employee k
        -> Map PM.UserId (a, P.Employee)
        -> Map PM.UserId Duration
        -> Map k (Map Duration Int)
    distrPer g pmMap as = Map.fromListWith (Map.unionWith (+))
        [ (e ^. g,  absencesToDistr $ as ^. ix pmUid)
        | (pmUid, (_login, e)) <- Map.toList pmMap
        ]

absencesToPerSickDays :: Duration -> Int -> PerSickDays Int
absencesToPerSickDays (Duration n) m'
    | n <= 0    = m ::: 0 ::: 0 ::: 0 ::: 0 ::: 0 ::: 0 ::: 0 ::: VNil
    | n <= 1    = 0 ::: m ::: 0 ::: 0 ::: 0 ::: 0 ::: 0 ::: 0 ::: VNil
    | n <= 2    = 0 ::: 0 ::: m ::: 0 ::: 0 ::: 0 ::: 0 ::: 0 ::: VNil
    | n <= 3    = 0 ::: 0 ::: 0 ::: m ::: 0 ::: 0 ::: 0 ::: 0 ::: VNil
    | n <= 5    = 0 ::: 0 ::: 0 ::: 0 ::: m ::: 0 ::: 0 ::: 0 ::: VNil
    | n <= 10   = 0 ::: 0 ::: 0 ::: 0 ::: 0 ::: m ::: 0 ::: 0 ::: VNil
    | n <= 30   = 0 ::: 0 ::: 0 ::: 0 ::: 0 ::: 0 ::: m ::: 0 ::: VNil
    | otherwise = 0 ::: 0 ::: 0 ::: 0 ::: 0 ::: 0 ::: 0 ::: m ::: VNil
  where
    m = Sum m'

absencesToDistr :: Duration -> Map Duration Int
absencesToDistr d = Map.singleton d 1

{-
-------------------------------------------------------------------------------
-- Median
-------------------------------------------------------------------------------

type Average = []
getAverage :: Average Double -> Double
getAverage = quantile 0.9  . sort
mkAverage :: Double -> Average Double
mkAverage = return

quantile :: Fractional a => Rational -> [a] -> a
quantile _ [] = 0
quantile q xs
    | q < 0     = head xs
    | q > 1     = last xs
    | d == 0    = xs !! n
    | otherwise =
        fromRational d *       (xs !! n)
      + fromRational (1 - d) * (xs !! (n + 1))
  where
    m = fromIntegral (length xs - 1) * q
    n = truncate m :: Int
    d = m - fromIntegral n
-}
