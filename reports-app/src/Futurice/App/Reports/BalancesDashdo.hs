{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Reports.BalancesDashdo (balancesRDashdo) where

import Control.Lens              (contains, (<&>), _4)
import Dashdo.Elements
import Dashdo.Rdash              (charts)
import Dashdo.Types
import Data.Aeson                (toJSON)
import Data.Ord                  (Down (..))
import Data.Time                 (addDays)
import Futurice.Integrations
       (Employee (..), Integrations, beginningOfPrev2Month,
       personioPlanmillMap, planmillEmployee, runIntegrations)
import Futurice.Prelude
import Futurice.Servant          (Cache, cachedIO)
import Futurice.Time
import Futurice.Tribe
import Lucid                     hiding (for_)
import Lucid.Bootstrap
import Lucid.Bootstrap3
import Numeric.Interval.NonEmpty ((...))
import Prelude ()

import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import qualified Graphics.Plotly as Plotly
import qualified Personio        as P
import qualified PlanMill        as PM

import Futurice.App.Reports.Balances (Balance (..), balanceForUser)
import Futurice.App.Reports.Config

-------------------------------------------------------------------------------
-- ???
-------------------------------------------------------------------------------

type Ctx = (Cache, Manager, Logger, Config)

-------------------------------------------------------------------------------
-- Kind
-------------------------------------------------------------------------------

data BalanceKind = BalanceUnder | BalanceNormal | BalanceOver | NonBalance
  deriving (Eq, Ord, Enum, Bounded, Generic)

instance NFData BalanceKind

balanceKind :: (Num a, Ord a) => NDT 'Hours a -> BalanceKind
balanceKind h
    | h < (-20) = BalanceUnder
    | h > 40    = BalanceOver
    | otherwise = BalanceNormal
{-# INLINE balanceKind #-}

balanceKindToText :: BalanceKind -> Text
balanceKindToText BalanceUnder  = "under"
balanceKindToText BalanceOver   = "over"
balanceKindToText BalanceNormal = "normal"
balanceKindToText NonBalance    = "dont-apply"

instance ToHtml BalanceKind where
    toHtmlRaw = toHtml
    toHtml BalanceNormal = "normal"
    toHtml NonBalance    = span_ [ style_ "color: gray" ] "dont-apply"
    toHtml BalanceUnder  = span_ [ style_ "color: orange" ] "under"
    toHtml BalanceOver   = span_ [ style_ "color: red" ] "over"

-------------------------------------------------------------------------------
-- Val
-------------------------------------------------------------------------------

type Contract = Text

data Val = Val
    { vName       :: !Text
    , vTribe      :: !Tribe
    , vContract   :: !Contract
    , vStatus     :: !P.Status
    , vSupervisor :: !Text
    , vFlex       :: !Double
    , vMissing    :: !Double
    , vKind       :: !BalanceKind
    }
  deriving (Generic)

instance NFData Val

fetchValues :: Ctx -> PM.Interval Day -> IO [Val]
fetchValues ctx interval = cachedIO' ctx interval $
    runIntegrations' ctx action
  where
    contractTypes =
        --Set.take 2 $
        Set.fromDistinctAscList . take 2 . Set.toAscList $
        cfgMissingHoursContracts (view _4 ctx)

    mkSupervisorMap es = Map.fromList $ flip mapMaybe es $ \e -> do
        sid   <- e ^. P.employeeSupervisorId
        sname <- Map.lookup sid m
        login <- e ^. P.employeeLogin
        pure (login, sname)
      where
        m :: Map P.EmployeeId Text
        m = Map.fromList $ map (\e -> (e ^. P.employeeId, e ^. P.employeeFullname)) es

    action = do
        -- people: do not include only some contracts
        us <- personioPlanmillMap
        supervisorMap <- mkSupervisorMap <$> P.personio P.PersonioEmployees
        today <- currentDay

        let inLastTwoMonths Nothing  = False
            inLastTwoMonths (Just d) =
                addDays (-60) today <= d && d <= today

        -- build
        valsMap <- ifor us $ \login (p, u) -> do
            let haveBalance = contractTypes ^. contains (PM.uContractType u)

            (Balance flex mh, e) <- (,)
                  <$> (if haveBalance
                      then balanceForUser interval u
                      else pure $ Balance 0 0)
                  <*> planmillEmployee (u ^. PM.identifier)

            let supervisor = fromMaybe "No supervisor" $ supervisorMap ^? ix login

            -- Show only active, or people with contract ended in last two months
            if p ^. P.employeeStatus /= P.Active && not (inLastTwoMonths (p ^. P.employeeEndDate))
            then pure Nothing
            else pure $ Just Val
                { vName       = employeeName e
                , vTribe      = employeeTribe e
                , vContract   = employeeContract e
                , vStatus     = p ^. P.employeeStatus
                , vSupervisor = supervisor
                , vFlex       = realToFrac $ unNDT flex
                , vMissing    = realToFrac $ unNDT mh
                , vKind       = if haveBalance
                                then balanceKind (flex + mh)
                                else NonBalance
                }

        -- strip Nothing:s
        pure (valsMap ^.. folded . _Just)

runIntegrations' :: Ctx -> Integrations [I, I, Proxy, I, I, I] a -> IO a
runIntegrations' (_, mgr, lgr, cfg) m = do
    now <- currentTime
    runIntegrations mgr lgr now (cfgIntegrationsCfg cfg) m

cachedIO' :: (Eq k, Hashable k, Typeable k, NFData v, Typeable v) => Ctx -> k -> IO v -> IO v
cachedIO' (cache, _, logger, _) = cachedIO logger cache 600

-------------------------------------------------------------------------------
-- Params
-------------------------------------------------------------------------------

data Params = Params
    { _pTribes      :: ![Text]
    , _pSupervisors :: ![Text]
    , _pKinds       :: ![Text]
    }
  deriving Show

makeLenses ''Params

params0 :: Params
params0 = Params [] [] []

-------------------------------------------------------------------------------
-- Parts
-------------------------------------------------------------------------------

tribePie :: [Val] -> SHtml IO Params ()
tribePie vs = plotlySelectMultiple plotly pTribes
  where
    plotly = Plotly.plotly "tribes" [trace]

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
        map (\Val {..} -> (vTribe, 1)) vs

    l :: [(Tribe, Double)]
    l = sortOn (Down . snd) $ Map.toList m

    pieLabels :: [Text]
    pieLabels = tribeToText . fst <$> l

    pieValues :: [Double]
    pieValues = snd <$> l

    pieColors = Plotly.List $ toJSON <$> zipWith const colors pieLabels

kindPie :: [Val] -> SHtml IO Params ()
kindPie vs = plotlySelectMultiple plotly pKinds
  where
    plotly = Plotly.plotly "kinds" [trace]

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

    m :: Map BalanceKind Double
    m = Map.fromListWith (+) $
        [ (k, 0) | k <- [ minBound .. maxBound ] ] ++
        map (\Val {..} -> (vKind, 1)) vs

    -- no sorting to get colors to align
    l :: [(BalanceKind, Double)]
    l = Map.toList m

    pieLabels :: [Text]
    pieLabels = balanceKindToText . fst <$> l

    pieValues :: [Double]
    pieValues = snd <$> l

    pieColors = Plotly.List $ toJSON <$> zipWith const kindColors pieLabels

valueTable :: [Val] -> SHtml IO Params ()
valueTable vs = table_ [ class_ "table table-striped" ] $ do
    thead_ $ tr_ $ do
        th_ "Name"
        th_ "Tribe"
        th_ "Contract"
        th_ "Status"
        th_ "Supervisor"
        th_ "Flex"
        th_ "Missing hours"
        th_ "Balance (flex + missing)"
        th_ mempty
    tbody_ $ for_ vs $ \Val {..} -> tr_ $ do
        td_ $ toHtml vName
        td_ $ toHtml vTribe
        td_ $ toHtml vContract
        td_ $ toHtml vStatus
        td_ $ toHtml vSupervisor
        td_ $ toHtml $ show vFlex
        td_ $ toHtml $ show vMissing
        td_ $ toHtml $ show (vFlex + vMissing)
        td_ $ toHtml vKind

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

supervisorBarPlot :: [Val] -> SHtml IO Params ()
supervisorBarPlot vs = plotlySelectMultiple plotly pSupervisors
  where
    plotly = Plotly.plotly "supervisors" [trace]
        & Plotly.layout . Plotly.height ?~ 1000

    trace :: Plotly.Trace
    trace = Plotly.hbarChart $ sortOn snd $ Map.toList m

    m :: Map Text Double
    m = Map.fromListWith (+) $ vs <&> \Val {..} ->
        (vSupervisor, 1)

kindColors :: [Plotly.RGB Int]
kindColors = cycle
    [ rgb 5 3 1
    , rgb 1 3 5
    , rgb 4 0 0
    , rgb 3 3 3
    ]
  where
    rgb r g b = Plotly.RGB (r * 51) (g * 51) (b * 51)

-------------------------------------------------------------------------------
-- RDashdo
-------------------------------------------------------------------------------

balancesRDashdo :: Ctx -> RDashdo IO
balancesRDashdo  ctx =
    RDashdo "balances" "Flex Balances" $ Dashdo params0 $ balances ctx

balances :: Ctx -> SHtml IO Params ()
balances ctx = do
    day <- lift $ currentDay
    let interval = beginningOfPrev2Month day ... day

    values <- liftIO $ fetchValues ctx interval
    params <- getValue

    -- filters
    let tribeFilter = case _pTribes params of
            [] -> id
            ts -> filter ((`elem` ts) . tribeToText . vTribe)

    let kindFilter = case _pKinds params of
            [] -> id
            ts -> filter ((`elem` ts) . balanceKindToText . vKind)

    let supervisorFilter = case _pSupervisors params of
            [] -> id
            ts -> filter ((`elem` ts) . vSupervisor)


    let valuesNoTribe = kindFilter . supervisorFilter $ values
    let valuesNoKind = tribeFilter . supervisorFilter $ values
    let valuesNoSupervisor = kindFilter . tribeFilter $ values

    let filteredValues = supervisorFilter valuesNoSupervisor

    row_ $ mkCol [(MD,12)] $ ul_ $ do
        li_ "People with no flex are also included for completeness"
        li_ "Balance is flex + missing hours, as when hours are marked flex goes up"
        li_ "Externals should belong to some tribe, but data is not yet up to date"
        li_ "Shows people Active in Personio, and ones with contractEndDate in last two months"

    -- charts
    charts
        [ ("Employees per tribe", tribePie valuesNoTribe)
        , ("Employees per balance", kindPie valuesNoKind)
        ]

    charts
        [ ("Employees per supervisor", supervisorBarPlot valuesNoSupervisor)
        ]

    -- table
    row_ $ mkCol [(MD,12)] $ do
        valueTable filteredValues
