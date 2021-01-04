{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.HC.Achoo.Render where

import Data.Time                 (diffDays)
import Futurice.Company          (Country, countryToText)
import Futurice.Prelude
import Futurice.Tribe            (Tribe, tribeToText)
import Numeric.Interval.NonEmpty (inf, sup)
import Prelude ()
import Servant                   (toQueryParam)
import Servant.Chart             (Chart (..))
import Text.Printf               (printf)

import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import qualified Futurice.Colour               as FC
import qualified Graphics.Rendering.Chart.Easy as C

import Futurice.App.HC.API
import Futurice.App.HC.Achoo.Types
import Futurice.App.HC.Markup

achooReportPage :: AchooReport -> HtmlPage "achoo-report"
achooReportPage report = page_ ("Achoo " <> textShow (arInterval report) <> " report") (Just NavAchoo) $ do
    ul_ $ do
        li_ "Report considers internal, active (for at least a day) during the period people"

    form_ [ method_ "GET" ] $ do
        dl_ $ do
            dd_ "From"
            dt_ $ input_ [ name_ "from", type_ "date", value_ $ toQueryParam dayMin ]
            dd_ "To"
            dt_ $ input_ [ name_ "to", type_ "date", value_ $ toQueryParam dayMax ]
            dd_ "Whole"
            dt_ $ select_ [ name_ "whole" ] $ do
                optionSelected_ whole       [ value_ "true" ]  "Include people who were active the whole interval"
                optionSelected_ (not whole) [ value_ "false" ] "Include people who were active in the interval (at least single day)"

        input_ [ class_ "button", type_ "Submit", value_ "Update" ]

    hr_ []
    ul_ [ class_ "menu" ] $ do
        li_ $ a_ [ recordHref_ recAchooReport (Just $(mkDay "2021-01-01")) (Just $(mkDay "2021-06-30")) (Just whole) ] $ "2021 H1"
        li_ $ a_ [ recordHref_ recAchooReport (Just $(mkDay "2021-01-01")) (Just $(mkDay "2021-12-31")) (Just whole) ] $ "2021 Year"
        li_ $ a_ [ recordHref_ recAchooReport (Just $(mkDay "2020-01-01")) (Just $(mkDay "2020-06-30")) (Just whole) ] $ "2020 H1"
        li_ $ a_ [ recordHref_ recAchooReport (Just $(mkDay "2020-07-01")) (Just $(mkDay "2020-12-31")) (Just whole) ] $ "2020 H2"
        li_ $ a_ [ recordHref_ recAchooReport (Just $(mkDay "2020-01-01")) (Just $(mkDay "2020-12-31")) (Just whole) ] $ "2020 Year"
        li_ $ a_ [ recordHref_ recAchooReport (Just $(mkDay "2019-01-01")) (Just $(mkDay "2019-06-30")) (Just whole) ] $ "2019 H1"
        li_ $ a_ [ recordHref_ recAchooReport (Just $(mkDay "2019-07-01")) (Just $(mkDay "2019-12-31")) (Just whole) ] $ "2019 H2"
        li_ $ a_ [ recordHref_ recAchooReport (Just $(mkDay "2019-01-01")) (Just $(mkDay "2019-12-31")) (Just whole) ] $ "2019 Year"
        li_ $ a_ [ recordHref_ recAchooReport (Just $(mkDay "2018-01-01")) (Just $(mkDay "2018-06-30")) (Just whole) ] $ "2018 H1"
        li_ $ a_ [ recordHref_ recAchooReport (Just $(mkDay "2018-07-01")) (Just $(mkDay "2018-12-31")) (Just whole) ] $ "2018 H2"
        li_ $ a_ [ recordHref_ recAchooReport (Just $(mkDay "2018-01-01")) (Just $(mkDay "2018-12-31")) (Just whole) ] $ "2018 Year"
    hr_ []

    h2_ "Percentages: Per tribe"
    img_ [ recordSrc_ recAchooChart ACTribe dayMin dayMax whole ]
    renderPercentages "Tribe" (arPercentsTribe report)

    h2_ "Percentages: Per office"
    img_ [ recordSrc_ recAchooChart ACOffice dayMin dayMax whole ]
    renderPercentages "Office" (arPercentsOffice report)

    h2_ "Percentages: Per country"
    img_ [ recordSrc_ recAchooChart ACCountry dayMin dayMax whole ]
    renderPercentages "Country" (countryKeyToText $ arPercentsCountry report)

    h2_ "Sickness days per person (averages)"

    h3_ "Per tribe"
    renderAverages "Tribe" (arAverageTribe report)

    h3_ "Per office"
    renderAverages "Office" (arAverageOffice report)

    h3_ "Per country"
    renderAverages "Country" (countryKeyToText $ arAverageCountry report)

  where
    dayMin = inf $ arInterval report
    dayMax = sup $ arInterval report
    whole  = arWhole report
    len = 1 + diffDays dayMax dayMin

    renderPercentages :: (Monad m, ToHtml a) => Text -> Map a (PerSickDays Int) -> HtmlT m ()
    renderPercentages title pers = sortableTable_ $ do
        thead_ $ do
            th_ $ toHtml title
            for_ tableBucketNames $ \n -> th_ $ toHtmlRaw n
            th_ "Total"

        tbody_ $ ifor_ pers $ \tribe abc' -> tr_ $ do
            let abc =  toTableBuckets abc'
            let total = getSum (fold abc)
            let showN x = do
                    toHtml (printf "%.02f" (fromIntegral x / fromIntegral total * 100 :: Double) :: String)
                    "% ("
                    toHtml (show x)
                    ")"

            td_ $ toHtml tribe
            for_ abc $ \(Sum x) -> td_ $ showN x
            td_ $ toHtml $ show total

    renderAverages :: forall m a. (Monad m, Ord a, ToHtml a) => Text -> Map a Double -> HtmlT m ()
    renderAverages title xs = table_ $ do
        thead_ $ do
            th_ $ toHtml title
            th_ "Average"
            th_ "Normalised average (in 180 days)"

        tbody_ $ ifor_ xs $ \tribe x -> tr_ $ do
            td_ $ toHtml tribe
            td_ $ toHtml (printf "%.02f" x :: String)
            td_ $ toHtml (printf "%.02f" (x / fromInteger len * 180) :: String)

achooRenderChart :: AchooChart -> AchooReport -> Chart "achoo-chart"
achooRenderChart ty report = case ty of
    ACTribe   -> Chart . C.toRenderable $ tribeChart "Per tribe"   (arPercentsTribe report)
    ACOffice  -> Chart . C.toRenderable $ chart "Per office"  (arPercentsOffice report)
    ACCountry -> Chart . C.toRenderable $ chart "Per country(without family company members)" (maybeCountryToCountry $ arPercentsCountry report)
  where
    tribeChart :: String -> Map Tribe (PerSickDays Int) -> C.EC (C.Layout C.PlotIndex Double) ()
    tribeChart title pers = do
        -- using custom indices to not have empty spaces on chart because of old tribes
        let indeces = Map.fromList $ (\(a,b) -> (b,a)) <$> C.addIndexes (Map.keys pers)
        -- full titles don't fit the chart
        let titles = second (T.unpack . T.take 12 . tribeToText) <$> C.addIndexes (Map.keys pers)
        C.layout_title C..= title
        colors
        b <- C.bars
            (toList bucketNames)
            [ (tribe, abc ^.. folded . getter f)
            | (tribe, abc) <- mapMaybe (\(a,b) -> (,) <$> Map.lookup a indeces <*> pure b) $ Map.toList pers
            , let total = getSum (fold abc)
            , let f (Sum x) = fromIntegral x / fromIntegral total * 100 :: Double
            ]
        C.plot $ return $ C.plotBars $ b
            & C.plot_bars_style .~ C.BarsStacked
        C.layout_x_axis . C.laxis_override C..= C.axisLabelsOverride titles
        C.layout_x_axis . C.laxis_style . C.axis_label_style . C.font_size C..= 7

    chart :: C.PlotValue x => String -> Map x (PerSickDays Int) -> C.EC (C.Layout x Double) ()
    chart title pers = do
        C.layout_title C..= title
        colors
        b <- C.bars
            (toList bucketNames)
            [ (tribe, abc ^.. folded . getter f)
            | (tribe, abc) <- Map.toList pers
            , let total = getSum (fold abc)
            , let f (Sum x) = fromIntegral x / fromIntegral total * 100 :: Double
            ]
        C.plot $ return $ C.plotBars $ b
            & C.plot_bars_style .~ C.BarsStacked

    colors = C.setColors $ cycle
        [ C.opaque $ FC.colourToDataColour c
        | c <- toList bucketColors
        ]

countryKeyToText :: Map (Maybe Country) a -> Map Text a
countryKeyToText = Map.mapKeys (maybe "No country" countryToText)

maybeCountryToCountry :: Map (Maybe Country) a -> Map Country a
maybeCountryToCountry m =
    let invertKey (Just k, v) = Just (k,v)
        invertKey (Nothing,_) = Nothing
    in Map.fromList $ catMaybes $ map invertKey $ Map.toList m
