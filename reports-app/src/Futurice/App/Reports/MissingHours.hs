{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- | Missing hours report
module Futurice.App.Reports.MissingHours (
    -- * Report
    MissingHoursReport,
    missingHoursReport,
    -- * Predicate
    missingHoursEmployeePredicate,
    missingHoursEmployeeNotificationPredicate,
    -- * Data
    MissingHour (..),
    -- * Logic
    missingHoursForUser,
    -- * Lenses
    missingHourDay,
    missingHourCapacity,
    -- ** Params
    mhpGenerated,
    mhpFromDay,
    mhpToDay,
    mhpTotalHours,
    mhpFumPublicUrl
    ) where

import Control.Lens              (sumOf)
import Data.Fixed                (Centi)
import Futurice.Constants        (fumPublicUrl)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Futurice.Report.Columns
import Futurice.Time
import Futurice.Time.Month       (dayToMonth)
import Numeric.Interval.NonEmpty (Interval, inf, sup)
import Prelude ()

import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as Map
import qualified Data.Tuple.Strict   as S
import qualified Data.Vector         as V
import qualified FUM
import qualified Personio            as P
import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ

-------------------------------------------------------------------------------
-- Predicate
-------------------------------------------------------------------------------

missingHoursEmployeePredicate :: Interval Day -> P.Employee -> Bool
missingHoursEmployeePredicate interval p = and
    [ p ^. P.employeeEmploymentType == Just P.Internal
    , p ^. P.employeeSalaryType == Just P.Monthly
    , P.employeeIsActiveInterval interval p
    ]

missingHoursEmployeeNotificationPredicate :: Interval Day -> P.Employee -> Bool
missingHoursEmployeeNotificationPredicate interval p = and
    [ p ^. P.employeeEmploymentType == Just P.Internal
    , p ^. P.employeeSalaryType == Just P.Monthly
    , P.employeeEndsAfterInterval interval p
    , P.employeeIsActiveInterval interval p
    ]

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data MissingHour = MissingHour
    { _missingHourDay      :: !Day
    , _missingHourCapacity :: !(NDT 'Hours Centi)
    }
  deriving (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (NFData)

makeLenses ''MissingHour
deriveGeneric ''MissingHour
deriveVia [t| ToJSON MissingHour   `Via` Sopica MissingHour |]
deriveVia [t| FromJSON MissingHour `Via` Sopica MissingHour |]
instance ToSchema MissingHour where declareNamedSchema = sopDeclareNamedSchema

instance ToColumns MissingHour where
    type Columns MissingHour = '[Day, Month, NDT 'Hours Centi]
    toColumns (MissingHour d c) = [I d :* I (dayToMonth d) :* I c :* Nil]
    columnNames _ =
        K "day" :*
        K "month" :*
        K "capacity" :*
        Nil

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

type MissingHoursReport = Report
    "Missing hour markings (internal; salary: Monthly)"
    MissingHoursParams
    (HashMap FUM.Login :$ StrictPair Employee :$ Vector :$ MissingHour)

data MissingHoursParams = MissingHoursParams
    { _mhpGenerated    :: !UTCTime
    , _mhpFromDay      :: !Day
    , _mhpToDay        :: !Day
    , _mhpTotalHours   :: !(NDT 'Hours Centi)
    , _mhpFumPublicUrl :: !Text
    }
  deriving (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (NFData)

deriveGeneric ''MissingHoursParams
makeLenses ''MissingHoursParams
deriveVia [t| ToJSON MissingHoursParams   `Via` Sopica MissingHoursParams |]
deriveVia [t| FromJSON MissingHoursParams `Via` Sopica MissingHoursParams |]
instance ToSchema MissingHoursParams where declareNamedSchema = sopDeclareNamedSchema

instance ToHtml MissingHoursParams where
    toHtmlRaw = toHtml
    toHtml MissingHoursParams {..} = dl_ $ do
        dd_ $ do
            "Generated at "
            i_ "(Note: PlanMill data is updated nightly)"
        dt_ $ toHtml $ formatHumanHelsinkiTime _mhpGenerated

        dd_ "Total hours missing"
        dt_ $ toHtml $ show $ unNDT _mhpTotalHours

        dd_ "Interval"
        dt_ $ toHtmlRaw $ show _mhpFromDay <> " &mdash; " <> show _mhpToDay

        dd_ "Note"
        dt_ $ i_ "PlanMill data is updated at night around 03:00."

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

-- | TODO: use applicative
--
-- /TODO:/ should return Map Day DoubleMissingHour?
missingHoursForUser
    :: MonadPlanMillQuery m
    => PM.Interval Day
    -> PM.User
    -> m (Vector MissingHour)
missingHoursForUser interval user = do
    let uid = user ^. PM.identifier
    tr <- PMQ.timereports interval uid
    uc <- PMQ.capacities interval uid
    -- Show only missing hours after hireDate.
    -- Contrary to the name, hire date in Planmill isn't always trustworthy,
    -- but it's good enough for the purpose of this report.
    let uc' = V.filter (\c -> fromMaybe True $ (PM.userCapacityDate c >=) <$> PM.uHireDate user) uc
    pure $ mkMissingHours tr uc'
  where
    mkMissingHours :: PM.Timereports -> PM.UserCapacities -> Vector MissingHour
    mkMissingHours tr uc
        = V.fromList
        . map (uncurry MissingHour)
        . Map.toList
        $ Map.differenceWith minus uc' tr'
      where
        tr' :: Map Day (NDT 'Hours Centi)
        tr' = Map.fromListWith (+)
            . map (\x -> (PM.trStart x, ndtConvert' $ PM.trAmount x))
            . toList
            $ tr

        uc' :: Map Day (NDT 'Hours Centi)
        uc' = Map.fromList
            . filter (isPositive . snd)
            . map (\x -> (PM.userCapacityDate x, ndtConvert' $ PM.userCapacityAmount x))
            . toList
            $ uc

    -- For now show only days without any hour markings
    minus :: NDT 'Hours Centi -> NDT 'Hours Centi -> Maybe (NDT 'Hours Centi)
    minus a b
        | b > 0      = Nothing
        | otherwise  = Just a

    isPositive :: (Num a, Ord a) => a -> Bool
    isPositive = (>0)

missingHoursReport
    :: forall m. (PM.MonadTime m, MonadPlanMillQuery m, MonadPersonio m)
    => (PM.Interval Day -> P.Employee -> Bool) -- ^ predicate to include people on the report
    -> PM.Interval Day
    -> m MissingHoursReport
missingHoursReport predicate interval = do
    now <- currentTime

    fpm0 <- personioPlanmillMap
    let fpm1 = HM.filter (predicate interval . fst) fpm0

    fpm2 <- traverse (uncurry perUser) fpm1
    let total = sumOf (folded . folded . folded . missingHourCapacity) fpm2

    pure $ Report (MissingHoursParams now (inf interval) (sup interval) total (fumPublicUrl <> "/")) fpm2
  where
    perUser :: P.Employee -> PM.User -> m (StrictPair Employee :$ Vector :$ MissingHour)
    perUser pEmployee pmUser = (S.:!:)
        <$> planmillEmployee (pmUser ^. PM.identifier)
        <*> missingHoursForUser interval' pmUser
      where
        -- shrink interval with end date, if it exists
        interval' = i PM.... s
        i = inf interval & mcase (pEmployee ^. P.employeeHireDate) id max
        s = sup interval & mcase (pEmployee ^. P.employeeEndDate) id min
