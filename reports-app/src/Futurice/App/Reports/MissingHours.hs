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
    missingHoursSimplifiedReport,
    MissingHoursSimplifiedReport,
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
import Futurice.Time.Month
import Futurice.Tribe
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

data MissingHoursSimplifiedReport = MissingHoursSimplifiedReport
    { _mhsParams         :: !MissingHoursParams
    , _mhsData           :: !(HashMap FUM.Login :$ StrictPair Employee :$ Vector :$ MissingHour)
    , _mhsFilterEmployee :: !(Maybe Text)
    , _mhsFilterMonth    :: !(Maybe Month)
    , _mhsFilterTribe    :: !(Maybe Tribe)
    , _mhsWholeInterval  :: !(PM.Interval Day)
    , _mhsAllEmployees   :: ![Employee]
    } deriving (NFData, Generic, ToSchema)

missingHoursSimplifiedReport
  :: forall m. (PM.MonadTime m, MonadPlanMillQuery m, MonadPersonio m)
  => (PM.Interval Day -> P.Employee -> Bool)
  -> PM.Interval Day
  -> PM.Interval Day
  -> Maybe Text
  -> Maybe Month
  -> Maybe Tribe
  -> m MissingHoursSimplifiedReport
missingHoursSimplifiedReport predicate interval wholeInterval memp mmonth mtribe = do
    now <- currentTime

    fpm0 <- personioPlanmillMap
    let fpm1 = HM.filter (predicate interval . fst) fpm0

    fpm1' <- traverse (uncurry perUser) fpm1
    let fpm2' = maybe fpm1' (\emp -> HM.filter (\(e S.:!: _) -> employeeName e == emp) fpm1') memp
    let fpm2 = maybe fpm2' (\emp -> HM.filter (\(e S.:!: _) -> employeeTribe e == emp) fpm2') mtribe
    let total = sumOf (folded . folded . folded . missingHourCapacity) fpm2
    let employees =
            catMaybes $ fmap (\(_, employee S.:!: vecHours) -> if (sum $ _missingHourCapacity <$> vecHours) > 0 then Just employee else Nothing ) $ HM.toList fpm1'

    pure $ MissingHoursSimplifiedReport
        (MissingHoursParams now (inf interval) (sup interval) total (fumPublicUrl <> "/"))
        fpm2
        memp
        mmonth
        mtribe
        wholeInterval
        employees
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

instance ToHtml MissingHoursSimplifiedReport where
    toHtmlRaw = toHtml
    toHtml = toHtml . renderMissingHoursSimplifiedReport

renderMissingHoursSimplifiedReport :: MissingHoursSimplifiedReport -> HtmlPage "missing-hours-simplified"
renderMissingHoursSimplifiedReport (MissingHoursSimplifiedReport params d memp mmonth mtribe wholeInterval employees) = page_ "Missing hours" $ do
    fullRow_ $ h1_ "Missing hours markings (internal; salary; Monthly)"
    fullRow_ $ toHtml params
    form_ $ div_ [ class_ "row" ] $ do
        div_ [ class_ "columns medium-3" ] $ select_ [ name_ "month", data_ "futu-id" "missing-hours-month" ] $ do
            optionSelected_ (mmonth == Nothing) [ value_ "-"] $
                toHtml (dayToMonth $ params ^. mhpFromDay)
                <> toHtml (tid " - ")
                <> toHtml (dayToMonth $ params ^. mhpToDay)
            for_ [ (dayToMonth $ inf wholeInterval ) .. (dayToMonth $ sup wholeInterval) ] $ \m ->
              optionSelected_ (Just m == mmonth) [ value_ $ monthToText m ] $ toHtml m
        div_ [ class_ "columns medium-5" ] $ select_ [ name_ "employee", data_ "futu-id" "missing-hours-employee" ] $ do
            optionSelected_ (memp == Nothing) [ value_ "-"] "All employees"
            for_ (sortOn employeeName employees) $ \employee ->
              optionSelected_ (Just (employeeName employee) == memp) [ value_ $ employeeName employee ] $ toHtml $ employeeName employee
        div_ [ class_ "columns medium-3" ] $ select_ [ name_ "tribe", data_ "futu-id" "missing-hours-tribe" ] $ do
            optionSelected_ (mtribe == Nothing) [ value_ "-"] "All tribes"
            for_ [ minBound .. maxBound ] $ \tribe ->
                optionSelected_ (Just tribe == mtribe) [value_ $ tribeToText tribe ] $ toHtml tribe
        div_ [ class_ "columns medium-2" ] $ input_ [ class_ "button", type_ "submit", value_ "Update" ]

    fullRow_ $ do
        h2_ "Missing hours by employee"
        sortableTable_ [data_ "futu-id" "missing-hours-by-employee-table"] $ do
            thead_ $ do
                th_ $ "Name"
                th_ $ "Hours"
            tbody_ $ for_ d $ \(employee S.:!: vecHours) ->
                when ((sum $ _missingHourCapacity <$> vecHours) > 0) $ tr_ $ do
                    td_ $ toHtml $ employeeName employee
                    td_ $ toHtml $ sum $ _missingHourCapacity <$> vecHours
                    td_ [style_ "display:none"] $ toHtml $ employeeTribe employee
        h2_ "All missing days"
        sortableTable_ [data_ "futu-id" "missing-hours-all"] $ do
            thead_ $ do
                th_ $ "Name"
                th_ $ "Hours"
                th_ $ "Date"
            tbody_ $ for_ d $ \(employee S.:!: vecHours) ->
              for_ vecHours $ \ a -> tr_ $ do
                td_ $ toHtml $ employeeName employee
                td_ $ toHtml $ _missingHourCapacity a
                td_ $ toHtml $ show $ _missingHourDay a
                td_ [style_ "display:none"] $ toHtml $ employeeTribe employee

  where
      tid :: Text -> Text
      tid = id
