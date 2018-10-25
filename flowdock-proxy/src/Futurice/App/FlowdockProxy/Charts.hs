{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FlowdockProxy.Charts (
    chartsPage,
    activityChart,
    ) where

import Control.Lens               ((.=))
import Control.Monad.State.Strict (StateT, evalStateT, state)
import Futurice.Prelude
import Prelude ()
import Servant.Chart              (Chart (..))

import qualified Data.Map.Strict               as Map
import qualified Graphics.Rendering.Chart.Easy as C

import Futurice.App.FlowdockProxy.API
import Futurice.App.FlowdockProxy.Markup

chartsPage :: HtmlPage "charts"
chartsPage = page_ "Charts" (Just NavCharts) $ do
    h2_ "Activity"
    img_ [ recordSrc_ recChartActivity ]

activityChart :: Map Text (Map Int Int) -> Chart "activity"
activityChart xss = Chart . C.toRenderable $ do
    C.layout_title .= "Activity by (Finnish) hour (last 6 months)"

    flip evalStateT lineStyles $ ifor_ xss $ \name xs -> do
        let values' =
              [ (h, fromMaybe 0 $ Map.lookup h xs)
              | h <- [ 0 .. 23 ]
              ]
        let total = sum (map snd values')
        let values :: [(Int, Double)]
            values =
              [ (h, fromIntegral x / fromIntegral total)
              | (h, x) <- values'
              ]

        style <- nextLineStyle
        lift $ C.plot $ line' style (name ^. unpacked) [values]

    C.layout_x_axis . C.laxis_title .= "Hour"
    C.layout_y_axis . C.laxis_title .= "Relative activity"

-- TODO: same as in MissingHoursChart
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
