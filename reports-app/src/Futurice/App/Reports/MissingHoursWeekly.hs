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
module Futurice.App.Reports.MissingHoursWeekly (
    -- * Report
    MissingHoursWeeklyTitle,
    MissingHoursWeeklyReport,
    missingHoursWeeklyReport,
    -- * Data
    MissingHour (..),
    -- * Logic
    missingHoursForUser,
    -- ** Params
    mhpGenerated,
    mhpFromDay,
    mhpToDay,
    ) where

import Data.Fixed                  (Centi)
import Data.Time.Calendar.WeekDate (fromWeekDate, toWeekDate)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Futurice.Report.Columns
import Futurice.Time
import Numeric.Interval.NonEmpty   (inf, sup, (...))
import Prelude ()

import qualified Data.Csv            as Csv
import qualified Data.HashMap.Strict as HM
import qualified Data.Map            as Map
import qualified Data.Tuple.Strict   as S
import qualified Data.Vector         as V
import qualified FUM
import qualified Personio            as P
import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ

-------------------------------------------------------------------------------
-- Week
-------------------------------------------------------------------------------

data Week = Week !Integer !Int
  deriving stock (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (NFData)

dayToWeek :: Day -> Week
dayToWeek d = case toWeekDate d of
    (y, w, _) -> Week y w

firstDayOfWeek :: Week -> Day
firstDayOfWeek (Week y w) = fromWeekDate y w 1

instance ToJSON Week where
    toJSON = toJSON . firstDayOfWeek
instance FromJSON Week where
    parseJSON = fmap dayToWeek . parseJSON

instance ToSchema Week -- fix me
instance ReportValue Week where
    reportValueHtml w = "W: " <> day_ (firstDayOfWeek w)

instance Csv.ToField Week where
    toField = Csv.toField . firstDayOfWeek

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data MissingHour = MissingHour
    { _missingHourDay      :: !Week
    , _missingHourMarked   :: !(NDT 'Hours Centi)
    , _missingHourCapacity :: !(NDT 'Hours Centi)
    }
  deriving stock (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (NFData)

-- makeLenses ''MissingHour
deriveGeneric ''MissingHour
deriveVia [t| ToJSON MissingHour   `Via` Sopica MissingHour |]
deriveVia [t| FromJSON MissingHour `Via` Sopica MissingHour |]
instance ToSchema MissingHour where declareNamedSchema = sopDeclareNamedSchema

instance ToColumns MissingHour where
    type Columns MissingHour = '[Week, NDT 'Hours Centi, NDT 'Hours Centi]
    columnNames _ = K "week starting" :* K "marked" :* K "capacity" :* Nil
    toColumns (MissingHour d m c) = [I d :* I m :* I c :* Nil]

-------------------------------------------------------------------------------
-- Report
-------------------------------------------------------------------------------

type MissingHoursWeeklyTitle = "Missing hour markings, weekly"

type MissingHoursWeeklyReport title = Report
    title
    MissingHoursWeeklyParams
    (HashMap FUM.Login :$ StrictPair Employee :$ Vector :$ MissingHour)

data MissingHoursWeeklyParams = MissingHoursWeeklyParams
    { _mhpGenerated    :: !UTCTime
    , _mhpFromDay      :: !Day
    , _mhpToDay        :: !Day
    }
  deriving (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (NFData)

deriveGeneric ''MissingHoursWeeklyParams
makeLenses ''MissingHoursWeeklyParams
deriveVia [t| ToJSON MissingHoursWeeklyParams   `Via` Sopica MissingHoursWeeklyParams |]
deriveVia [t| FromJSON MissingHoursWeeklyParams `Via` Sopica MissingHoursWeeklyParams |]
instance ToSchema MissingHoursWeeklyParams where declareNamedSchema = sopDeclareNamedSchema

instance ToHtml MissingHoursWeeklyParams where
    toHtmlRaw = toHtml
    toHtml MissingHoursWeeklyParams {..} = dl_ $ do
        dd_ $ do
            "Generated at "
            i_ "(Note: data is pulled from caches, so it is few hours old at worst)"
        dt_ $ toHtml $ formatHumanHelsinkiTime _mhpGenerated

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
    pure $ mkMissingHoursWeekly tr uc'
  where
    mkMissingHoursWeekly :: PM.Timereports -> PM.UserCapacities -> Vector MissingHour
    mkMissingHoursWeekly tr uc
        = V.fromList
        $ map (\(w, (r, c)) -> MissingHour w r c)
        $ Map.toList
        $ Map.mapMaybe id
        $ alignWith minus uc' tr'
      where
        tr' :: Map Week (NDT 'Hours Centi)
        tr' = Map.fromListWith (+)
            . map (\x -> (dayToWeek $ PM.trStart x, ndtConvert' $ PM.trAmount x))
            . toList
            $ tr

        uc' :: Map Week (NDT 'Hours Centi)
        uc' = Map.filter isPositive
            . Map.fromListWith (+)
            . map (\x -> (dayToWeek $ PM.userCapacityDate x, ndtConvert' $ PM.userCapacityAmount x))
            . toList
            $ uc

    -- For now show only days without any hour markings
    minus :: These (NDT 'Hours Centi) (NDT 'Hours Centi) -> Maybe (NDT 'Hours Centi, NDT 'Hours Centi)
    minus (That _r)  = Nothing
    minus (This c)   = Just (0 ,c)
    minus (These c r)
        | r >= c     = Nothing
        | otherwise  = Just (r, c)

    isPositive :: (Num a, Ord a) => a -> Bool
    isPositive = (>0)

missingHoursWeeklyReport
    :: forall m title. (PM.MonadTime m, MonadPlanMillQuery m, MonadPersonio m)
    => (PM.Interval Day -> P.Employee -> Bool) -- ^ predicate to include people on the report
    -> PM.Interval Day
    -> m (MissingHoursWeeklyReport title)
missingHoursWeeklyReport predicate intervalD = do
    now <- currentTime

    fpm0 <- personioPlanmillMap
    let fpm1 = HM.filter (predicate interval . fst) fpm0

    fpm2 <- traverse (uncurry perUser) fpm1

    pure $ Report (MissingHoursWeeklyParams now (inf interval) (sup interval)) fpm2
  where
    interval = f (inf intervalD) ... f (sup intervalD) where f = firstDayOfWeek . dayToWeek

    perUser :: P.Employee -> PM.User -> m (StrictPair Employee :$ Vector :$ MissingHour)
    perUser pEmployee pmUser = (S.:!:)
        <$> planmillEmployee (pmUser ^. PM.identifier)
        <*> missingHoursForUser interval' pmUser
      where
        -- shrink interval with end date, if it exists
        interval' = i PM.... s
        i = inf interval & mcase (pEmployee ^. P.employeeHireDate) id max
        s = sup interval & mcase (pEmployee ^. P.employeeEndDate) id min
