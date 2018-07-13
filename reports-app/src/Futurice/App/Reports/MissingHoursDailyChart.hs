{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Reports.MissingHoursDailyChart (
    MissingHoursDailyChartData,
    missingHoursDailyChartData,
    missingHoursDailyChartRender,
    ) where

import Control.Lens                ((.=), (^@..))
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Colour
import Prelude ()
import Servant.Chart               (Chart (..))

import Futurice.App.Reports.Ctx

import qualified Data.Map.Strict               as Map
import qualified Graphics.Rendering.Chart.Easy as C

type MissingHoursDailyChartData = Map Day (Double, Double)

missingHoursDailyChartData
    :: Ctx
    -> LogT IO MissingHoursDailyChartData
missingHoursDailyChartData ctx = do
    xs <- safePoolQuery_ ctx "SELECT day, last_friday, yesterday FROM reports.missing_hours"
    return $ Map.fromList [ (k, (x, y)) | (k, x, y) <- xs ]

missingHoursDailyChartRender :: MissingHoursDailyChartData -> Chart "missing-hours-daily"
missingHoursDailyChartRender m = Chart . C.toRenderable $ do
    C.layout_title .= "Missing hours at each day"
    C.plot $ line' lineStyle1 "from two previous months until last Friday"
        $ singleton $ m ^@.. ifolded . getter fst
    C.plot $ line' lineStyle2 "from two previous months until yesteday"
        $ singleton $ m ^@.. ifolded . getter snd

singleton :: a -> [a]
singleton x = [x]

line' :: C.LineStyle -> String -> [[(x,y)]]  -> C.EC l (C.PlotLines x y)
line' ls title values = C.liftEC $ do
    C.plot_lines_title  .= title
    C.plot_lines_values .= values
    C.plot_lines_style  .= ls

lineStyle1 :: C.LineStyle
lineStyle1 = C.def
    & C.line_color  .~ C.opaque (colourToDataColour (FutuAccent AF4 AC3))

lineStyle2 :: C.LineStyle
lineStyle2 = C.def
    & C.line_color  .~ C.opaque (colourToDataColour (FutuAccent AF2 AC3))

