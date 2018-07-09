{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Reports.MissingHoursDailyChart (
    MissingHoursDailyChartData,
    missingHoursDailyChartData,
    missingHoursDailyChartRender,
    ) where

import Control.Lens                ((.=))
import Futurice.Postgres
import Futurice.Prelude
import Prelude ()
import Servant.Chart               (Chart (..))

import Futurice.App.Reports.Ctx

import qualified Data.Map.Strict               as Map
import qualified Graphics.Rendering.Chart.Easy as C

type MissingHoursDailyChartData = Map Day Double

missingHoursDailyChartData
    :: Ctx
    -> LogT IO MissingHoursDailyChartData
missingHoursDailyChartData ctx = do
    xs <- safePoolQuery_ ctx "SELECT day, hours FROM reports.missing_hours"
    return (Map.fromList xs)

missingHoursDailyChartRender :: MissingHoursDailyChartData -> Chart "missing-hours-daily"
missingHoursDailyChartRender m = Chart . C.toRenderable $ do
    C.layout_title .= "Missing hours at each day"
    C.plot $ line' lineStyle "missing hours" $ singleton $
        Map.toList m

singleton :: a -> [a]
singleton x = [x]

line' :: C.LineStyle -> String -> [[(x,y)]]  -> C.EC l (C.PlotLines x y)
line' ls title values = C.liftEC $ do
    C.plot_lines_title .= title
    C.plot_lines_values .= values
    C.plot_lines_style .= ls

lineStyle :: C.LineStyle
lineStyle = C.def
    & C.line_color  .~ C.opaque C.red
    & C.line_dashes .~ []

