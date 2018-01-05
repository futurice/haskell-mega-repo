{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}
module Futurice.App.Reports.MissingHoursChart (
    MissingHoursChartData,
    missingHoursChartData,
    missingHoursChartRender,
    ) where

import Control.Lens                (contains, (.=))
import Control.Monad.State.Strict  (StateT, evalStateT, state)
import Data.Fixed                  (Centi)
import Data.Time.Calendar.WeekDate (fromWeekDate, toWeekDate)
import Futurice.Integrations
import Futurice.Prelude
import Futurice.Time
import Futurice.Tribe              (Tribe, tribeToText)
import Numeric.Interval.NonEmpty   ((...))
import Prelude ()
import Servant.Chart               (Chart (..))

import Futurice.App.Reports.MissingHours

import qualified Data.Map.Strict               as Map
import qualified Graphics.Rendering.Chart.Easy as C
import qualified PlanMill                      as PM

type MissingHoursChartData = (Integer, Int, Integer, Int, PM.Interval Day, Map Tribe (Sum Count, Map YearWeek (NDT 'Hours Centi)))

missingHoursChartData
    :: forall m env.
        ( PM.MonadTime m, MonadFUM m, MonadPlanMillQuery m, MonadPersonio m
        , MonadReader env m, HasFUMEmployeeListName env
        )
    => Set (PM.EnumValue PM.User "contractType")
    -> m MissingHoursChartData
missingHoursChartData contractTypes = do
    -- interval: from beginning of the year
    today <- currentDay
    let (currYear, currWeek, _) = toWeekDate today
    let normaliseWeek year week
            | week < 1 = normaliseWeek (year - 1) (week + 52)
            | otherwise = (year, week)
    let (yearA, weekA) = normaliseWeek currYear (currWeek - 21)
        (yearB, weekB) = normaliseWeek currYear (currWeek - 1)
    let interval =
            fromWeekDate yearA weekA 1 ...
            fromWeekDate yearB weekB 7

    -- people: do not include only some contracts
    fpm0 <- snd <$$> fumPlanmillMap
    let fpm1 :: [PM.User]
        fpm1 = filter (\e -> contractTypes ^. contains (PM.uContractType e)) (toList fpm0)

    -- timereports
    trs' <- for fpm1 $ \u -> (,)
        <$> planmillEmployee (u ^. PM.identifier)
        <*> missingHoursForUser interval u
    let trs = arrangeReports trs'

    pure (yearA, weekA, yearB, weekB, interval, trs)

missingHoursChartRender :: MissingHoursChartData -> Chart "missing-hours"
missingHoursChartRender (yearA, weekA, yearB, weekB, interval, trs) = Chart . C.toRenderable $ do
    C.layout_title .= "Missing hours per employee per week: " ++ show interval
    flip evalStateT lineStyles $ ifor_ trs $ \tribe (count, hours) -> do
        let scale :: NDT 'Hours Centi -> Double
            scale x = realToFrac (getNDT x) / fromIntegral (getSum count)
        lineStyle <- nextLineStyle
        lift $ C.plot $ line' lineStyle (tribeToText tribe ^. unpacked) $ singleton $ do
            (year, week) <- weeks
            let day = fromWeekDate year week 1
            pure (day, maybe 0 scale $ hours ^? ix (year, week))
  where
    weeks
      | yearA == yearB = [ (yearA, w) | w <- [ weekA .. weekB ] ]
      | otherwise =
          [ (yearA, w) | w <- [ weekA .. 52 ] ] ++
          [ (y,     w) | y <- [ yearA + 1 .. yearB - 1 ], w <- [1, 52] ] ++
          [ (yearB, w) | w <- [ 1 .. weekB ] ]

type Count = Int
type YearWeek = (Integer, Int)

arrangeReports
    :: [(Employee, Vector MissingHour)]
    -> Map Tribe (Sum Count, Map YearWeek (NDT 'Hours Centi))
arrangeReports = Map.fromListWith f . map process
  where
    -- I wish we had better Monoid (Map k v)
    f (x, m) (x', m') = (x <> x', Map.unionWith (+) m m')

    process (e, hours) =
        ( employeeTribe e
        , (Sum 1, Map.fromListWith (+) $ map process2 $ toList hours)
        )
    process2 mh = ((y, w), mh ^. missingHourCapacity)
      where
        (y, w, _) = toWeekDate (mh ^. missingHourDay)


singleton :: a -> [a]
singleton x = [x]

-- Move to Futurice.Time
getNDT :: NDT tu x -> x
getNDT (NDT x) = x

line' :: C.LineStyle -> String -> [[(x,y)]]  -> C.EC l (C.PlotLines x y)
line' lineStyle title values = C.liftEC $ do
    C.plot_lines_title .= title
    C.plot_lines_values .= values
    C.plot_lines_style .= lineStyle

lineStyles :: [C.LineStyle]
lineStyles = cycle
    [ C.def & C.line_color .~ C.opaque c & C.line_dashes .~ d
    | d <- ds, c <- cs
    ]
  where
    cs = [ C.red, C.blue, C.green, C.magenta, C.orange ]
    ds = [[], [1,1], [5,5]]

nextLineStyle ::  Monad m => StateT [C.LineStyle] m C.LineStyle
nextLineStyle = state $ \s -> case s of
    (x:xs) -> (x, xs)
    []     -> (C.def, [])
