{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Reports.MissingHoursDashdo (missingHoursRDashdo) where

import Control.Lens              ((<&>))
import Dashdo.Elements
import Dashdo.Rdash              (charts)
import Dashdo.Types
import Data.Aeson                (toJSON)
import Data.Ord                  (Down (..))
import Futurice.Integrations
import Futurice.Prelude
import Futurice.Servant          (Cache, cachedIO)
import Futurice.Time             (unNDT)
import Futurice.Tribe
import Lucid                     hiding (for_)
import Lucid.Bootstrap
import Lucid.Bootstrap3
import Numeric.Interval.NonEmpty ((...))
import Prelude ()

import qualified Data.Map        as Map
import qualified Graphics.Plotly as Plotly
import qualified Personio        as P
import qualified PlanMill        as PM

import Futurice.App.Reports.Config
import Futurice.App.Reports.MissingHours

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

type Ctx = (Cache, Manager, Logger, Config)

type Contract = Text

data Val = Val
    { vName         :: !Text
    , vTribe        :: !Tribe
    , vContract     :: !Contract
    , vDay          :: !Day
    , vMissingHours :: !Double
    }
  deriving (Generic)

instance NFData Val

fetchValues :: Ctx -> PM.Interval Day -> IO [Val]
fetchValues ctx interval = cachedIO' ctx interval $
    runIntegrations' ctx action
  where
    employeePredicate p = and
        [ p ^. P.employeeEmploymentType == Just P.Internal
        ]

    action = do
        -- people: include only some contracts
        fpm0 <- personioPlanmillMap
        let fpm1 :: [PM.User]
            fpm1 = map snd $ filter (employeePredicate . fst) $ toList fpm0

        -- timereports
        trs' <- for fpm1 $ \u -> (,)
            <$> planmillEmployee (u ^. PM.identifier)
            <*> missingHoursForUser interval u

        pure (foldMap toVal trs')

    toVal :: (Employee, Vector MissingHour) -> [Val]
    toVal (e, mhs) = f <$> toList mhs where
        f mh = Val
            { vName         = employeeName e
            , vTribe        = employeeTribe e
            , vContract     = employeeContract e
            , vDay          = _missingHourDay mh
            , vMissingHours = realToFrac $ unNDT $ _missingHourCapacity mh
            }

runIntegrations' :: Ctx -> Integrations '[ ServFD, ServFUM, ServGH, ServPE, ServPM ] a -> IO a
runIntegrations' (_, mgr, lgr, cfg) m = do
    now <- currentTime
    runIntegrations mgr lgr now (cfgIntegrationsCfg cfg) m

cachedIO' :: (Eq k, Hashable k, Typeable k, NFData v, Typeable v) => Ctx -> k -> IO v -> IO v
cachedIO' (cache, _, logger, _) = cachedIO logger cache 600

-------------------------------------------------------------------------------
-- Params
-------------------------------------------------------------------------------

data Params = Params
    { _pTribes    :: ![Text]
    , _pContracts :: ![Contract]
    , _pDays      :: ![Text]
    }
  deriving Show

makeLenses ''Params

params0 :: Params
params0 = Params [] [] []

-------------------------------------------------------------------------------
-- Parts
-------------------------------------------------------------------------------

tribePie :: [Val] -> SHtml IO Params ()
tribePie vs = plotlySelectMultiple plotly' pTribes
  where
    plotly' = Plotly.plotly "tribes" [trace]

    trace :: Plotly.Trace
    trace = Plotly.pie
        & Plotly.labels ?~ pieLabels
        & Plotly.values ?~ map toJSON pieValues
        & Plotly.customdata ?~ map toJSON pieLabels
        & Plotly.hole ?~ toJSON (0.4 :: Double)
        & Plotly.marker ?~ marker

    marker :: Plotly.Marker
    marker = Plotly.defMarker
        & Plotly.markercolors ?~ pieColors

    m :: Map Tribe Double
    m = Map.fromListWith (+) $
        map (\Val {..} -> (vTribe, vMissingHours)) vs

    l :: [(Tribe, Double)]
    l = sortOn (Down . snd) $ Map.toList m

    pieLabels :: [Text]
    pieLabels = tribeToText . fst <$> l

    pieValues :: [Double]
    pieValues = snd <$> l

    pieColors = Plotly.List $ toJSON <$> zipWith const colors pieLabels

contractPie :: [Val] -> SHtml IO Params ()
contractPie vs = plotlySelectMultiple plotly' pContracts
  where
    plotly' = Plotly.plotly "contract" [trace]

    trace :: Plotly.Trace
    trace = Plotly.pie
        & Plotly.labels ?~ pieLabels
        & Plotly.values ?~ map toJSON pieValues
        & Plotly.customdata ?~ map toJSON pieLabels
        & Plotly.hole ?~ toJSON (0.4 :: Double)
        & Plotly.marker ?~ marker

    marker :: Plotly.Marker
    marker = Plotly.defMarker
        & Plotly.markercolors ?~ pieColors

    m :: Map Contract Double
    m = Map.fromListWith (+) $
        map (\Val {..} -> (vContract, vMissingHours)) vs

    l :: [(Contract, Double)]
    l = sortOn (Down . snd) $ Map.toList m

    pieLabels :: [Text]
    pieLabels = fst <$> l

    pieValues :: [Double]
    pieValues = snd <$> l

    pieColors = Plotly.List $ toJSON <$> zipWith const colors pieLabels

dayBarPlot :: [Val] -> SHtml IO Params ()
dayBarPlot vs = plotlySelectMultiple plotly pDays
  where
    plotly = Plotly.plotly "days" [trace]

    trace :: Plotly.Trace
    trace = Plotly.vbarChart $ map (first textShow) $ Map.toList m

    m = Map.fromListWith (+) $ vs <&> \Val {..} ->
        (vDay, vMissingHours)

colors :: [Plotly.RGB Int]
colors = cycle
    [ rgb 1 3 5
    , rgb 5 3 1
    , rgb 1 4 2
    , rgb 4 0 0
    , rgb 3 0 5
    , rgb 3 3 3
    ]
  where
    rgb r g b = Plotly.RGB (r * 51) (g * 51) (b * 51)

valueTable :: [Val] -> SHtml IO Params ()
valueTable vs = table_ [ class_ "table table-striped" ] $ do
    thead_ $ tr_ $ do
        th_ "Name"
        th_ "Tribe"
        th_ "Contract"
        th_ "Day"
        th_ "Hours"
    tbody_ $ for_ vs $ \Val {..} -> tr_ $ do
        td_ $ toHtml vName
        td_ $ toHtml vTribe
        td_ $ toHtml vContract
        td_ $ toHtml $ show vDay
        td_ $ toHtml (show vMissingHours)

-------------------------------------------------------------------------------
-- Dashdo
-------------------------------------------------------------------------------

missingHours :: Ctx -> SHtml IO Params ()
missingHours ctx = do
    day <- lift $ currentDay
    let interval = beginningOfPrev2Month day ... pred day

    values <- liftIO $ fetchValues ctx interval
    params <- getValue

    -- Filters
    let tribeFilter = case _pTribes params of
            [] -> id
            ts -> filter ((`elem` ts) . tribeToText . vTribe)

    let contractFilter = case _pContracts params of
            [] -> id
            ts -> filter ((`elem` ts) . vContract)

    let daysFilter = case _pDays params of
            [] -> id
            ts -> filter ((`elem` ts) . textShow . vDay)

    let filteredValues1 = daysFilter values
    let filteredValues2 = contractFilter . tribeFilter $ values
    let filteredValuesBoth = daysFilter filteredValues2

    row_ $ mkCol [(MD,12)] $ h2_ $ toHtml $ "from interval: " ++ show interval

    -- charts
    charts
        [ ("Missing hours per tribe", tribePie filteredValues1)
        , ("Missing hours per contract type", contractPie filteredValues1)
        ]

    charts
        [ ("Missing hours per day", dayBarPlot filteredValues2)
        ]

    -- table
    row_ $ mkCol [(MD,12)] $ do
        valueTable filteredValuesBoth

missingHoursRDashdo :: Ctx -> RDashdo IO
missingHoursRDashdo  ctx =
    RDashdo "missing-hours" "Missing Hours" $ Dashdo params0 $ missingHours ctx
