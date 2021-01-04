{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
module Futurice.App.Reports.UtzChart (utzChartData, utzChartRender) where

import Control.Lens                ((.=))
import Data.Time.Calendar.WeekDate (toWeekDate)
import Futurice.Integrations
import Futurice.Monoid             (Average (..))
import Futurice.Prelude
import Futurice.Time
import Numeric.Interval.NonEmpty   (Interval, inf, sup, (...))
import Prelude ()
import Servant.Chart               (Chart (..))

import qualified Data.Map.Strict               as Map
import qualified Graphics.Rendering.Chart.Easy as C
import qualified PlanMill                      as PM
import qualified PlanMill.Queries              as PMQ

data UTZChartData = UTZChartData
    { cdData         :: !(Map (Integer, Int) Double)
    , cdShowAllYears :: !Bool
    } deriving (Generic, NFData)

utzChartData
    :: forall m. ( MonadTime m, MonadPlanMillQuery m)
    => m UTZChartData
utzChartData = do
    today <- currentDay
    uids <- view PM.identifier <$$> PMQ.users
    trs' <- bindForM (chopInterval $ interval today) $ \i ->
        traverse (PMQ.timereports i) uids
    let trs = trs' ^.. folded . folded . folded
    pure $ UTZChartData (timereportUtzPerWeek trs) False
  where
    interval today = $(mkDay "2015-01-01") PM.... today

utzChartRender :: UTZChartData -> Chart "utz"
utzChartRender (UTZChartData utzs showAll) = Chart . C.toRenderable $ do
    C.layout_title .= "UTZ per week"

    -- add top color again to get consistent color for first year
    C.liftCState (C.colors %= (\colors -> head colors : colors))

    -- add dashes to the last 4 months to mark uncertainly
    C.plot $ do
        line <- C.line "2021" [takeLast 4 (yearData 2021)]
        pure $ line & C.plot_lines_style . C.line_dashes .~ [5,5]
    C.plot $ C.line "2021" [dropLast 3 (yearData 2021)]
    C.plot $ C.line "2020" [yearData 2020]
    C.plot $ C.line "2019" [yearData 2019]
    C.plot $ C.line "2018" [yearData 2018]
    when showAll $ do
        C.plot $ C.line "2017" [yearData 2017]
        C.plot $ C.line "2016" [yearData 2016]
        C.plot $ C.line "2015" [yearData 2015]
  where
    takeLast n = reverse . take n . reverse

    dropLast n = reverse . drop n . reverse

    yearData y = takeWhileMaybe
        (\w -> fmap ((,) $ WeekNumber w) $ utzs ^? ix (y, w))
        [1..53]

    takeWhileMaybe :: (a -> Maybe b) -> [a] -> [b]
    takeWhileMaybe f = go where
        go []     = []
        go (a:as) = case f a of
            Nothing -> []
            Just b  -> b : go as

-------------------------------------------------------------------------------
-- WeekNumber
-------------------------------------------------------------------------------

newtype WeekNumber = WeekNumber Int deriving (Eq, Ord)

instance C.PlotValue WeekNumber where
    toValue (WeekNumber i) = fromIntegral i
    fromValue = WeekNumber . clamp 1 53 . truncate
      where
        clamp mi ma x
            | x < mi    = mi
            | x > ma    = ma
            | otherwise = x

    autoAxis _ = C.makeAxis
        (map (\(WeekNumber i) -> show i))
        (labels, numbers [1..53], grids)
      where
        labels = numbers [1,5,9,    14,18,22,    27,31,35,    40,44,48,    53]
        grids  = numbers [1,5,9,13, 14,18,22,26, 27,31,35,39, 40,44,48,52, 53]
        numbers = map WeekNumber

-------------------------------------------------------------------------------
-- Extras
-------------------------------------------------------------------------------

-- bindForM and chopInterval used to cut the parallelism, as we ask "for everything"
-- TODO: move to integrations
bindForM :: Monad m => [a] -> (a -> m b) -> m [b]
bindForM [] _ = return []
bindForM (a:as) f = do
    b <- f a
    bs <- bindForM as f
    return (b : bs)

chopInterval :: (Ord a, Enum a) => Interval a -> [Interval a]
chopInterval i
    | s < 50    = [i]
    | otherwise = (mi ... md) : chopInterval (succ md ... ma)
  where
    mi = inf i
    md = toEnum (fromEnum mi + 50)
    ma = sup i
    s = fromEnum ma - fromEnum mi

timereportUtzPerWeek :: [PM.Timereport] -> Map (Integer, Int) Double
timereportUtzPerWeek = fmap getAverage . Map.fromListWith (<>) . fmap mk
  where
    mk :: PM.Timereport -> ((Integer, Int), Average Double)
    mk tr = ((y, w), tr ^. reportUtilizationAvg)
      where
        (y, w, _) = toWeekDate (PM.trStart tr)

-------------------------------------------------------------------------------
-- Copied from hours-api
-------------------------------------------------------------------------------

billableStatus :: Maybe PM.ProjectId -> PM.EnumValue e f -> EntryType
billableStatus Nothing _                = EntryTypeOther
billableStatus _       (PM.EnumValue 3) = EntryTypeNotBillable
billableStatus _       _                = EntryTypeBillable

reportUtilizationAvg :: Getter PM.Timereport (Average Double)
reportUtilizationAvg = getter $ \tr ->
    let NDT hours = ndtConvert' (PM.trAmount tr) :: NDT 'Hours Double
    in case billableStatus (PM.trProject tr) (PM.trBillableStatus tr) of
        EntryTypeBillable    -> Average hours 100
        EntryTypeNotBillable -> Average hours 0
        EntryTypeOther       -> mempty

data EntryType
    = EntryTypeBillable
    | EntryTypeNotBillable
    | EntryTypeOther
