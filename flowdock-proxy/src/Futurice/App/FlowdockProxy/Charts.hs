{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FlowdockProxy.Charts (
    chartsPage,
    activityChart,
    ) where

import Control.Lens     ((.=))
import Futurice.Prelude
import Prelude ()
import Servant.Chart    (Chart (..))

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

    ifor_ xss $ \name xs -> do
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

        C.plot $ C.line (name ^. unpacked) [values]

    C.layout_x_axis . C.laxis_title .= "Hour"
    C.layout_y_axis . C.laxis_title .= "Relative activity"
