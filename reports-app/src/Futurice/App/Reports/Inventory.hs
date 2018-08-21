{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Futurice.App.Reports.Inventory (
    -- * Report
    inventorySummaryData,
    inventorySummaryQuery,
    inventoryBalanceQuantiles,
    -- * Types
    InventorySummary (..),
    ) where

import Control.Lens              (_4, _5, _6)
import Data.Ord                  (Down (..))
import Data.Time                 (addDays)
import FUM.Types.Login           (Login)
import Futurice.Daily
import Futurice.Generics
import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Postgres         (HasPostgresPool, safePoolQuery_)
import Futurice.Prelude
import Prelude ()
import Servant.Chart             (Chart (..))

import qualified Data.Map.Strict               as Map
import qualified Futurice.Colour               as FC
import qualified Graphics.Rendering.Chart.Easy as C
import qualified Personio                      as P

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data InventorySummary = InventorySummary
    { isToday    :: Day
    , isActives  :: Daily [P.SimpleEmployee]
    , isBalances :: Map P.EmployeeId (P.Employee, Daily Double)
    }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

deriveGeneric ''InventorySummary
instance ToSchema InventorySummary where declareNamedSchema = sopDeclareNamedSchema
-- deriveVia [t| ToJSON InventorySummary   `Via` Sopica InventorySummary |]
-- deriveVia [t| FromJSON InventorySummary `Via` Sopica InventorySummary |]

-------------------------------------------------------------------------------
-- Fetch
-------------------------------------------------------------------------------

inventorySummaryQuery :: HasPostgresPool ctx => ctx -> LogT IO [(Login, Day, Double)]
inventorySummaryQuery ctx = safePoolQuery_ ctx
    "select u.username, o.created :: date, o.amount :: real from common_user u, common_operation o where o.owner_id = u.id and length(username) <= 5;"

inventorySummaryData
    :: forall m. (MonadTime m, MonadPersonio m, MonadPlanMillQuery m)
    =>  [(Login, Day, Double)]
    -> m InventorySummary
inventorySummaryData xs = do
    today  <- currentDay
    ps     <- personio P.PersonioEmployees
    ses    <- dailyFromMap' mempty
        . P.internSimpleEmployees (traverse . traverse)
        <$> personio P.PersonioSimpleEmployees
    return $ InventorySummary today ses $ balances ps
  where
    balances :: [P.Employee] -> Map P.EmployeeId (P.Employee, Daily Double)
    balances ps = Map.fromList
        [ (i, (p, daily))
        | p <- ps
        , let i = p ^. P.employeeId
        , let daily = maybe (pure 0) scan  $ do
                login <- p ^. P.employeeLogin
                ys ^? ix login
        ]
      where
        ys :: Map Login (Map Day Double)
        ys = Map.fromListWith (Map.unionWith (+))
            [ (i, Map.singleton d v)
            | (i, d, v) <- xs
            ]

        scan :: Map Day Double -> Daily Double
        scan = dailyFromMap 0 . Map.fromList . scan' 0 . Map.toList

        scan' :: Double -> [(a, Double)] -> [(a, Double)]
        scan' _ []            = []
        scan' x ((a, y) : zs) = let yx = y + x in yx `seq` (a, yx) : scan' yx zs

-------------------------------------------------------------------------------
-- Graph
-------------------------------------------------------------------------------

inventoryBalanceQuantiles :: InventorySummary -> Chart "inventory-quantiles"
inventoryBalanceQuantiles (InventorySummary today ses balances) = Chart . C.toRenderable $ do
    C.setColors $ cycle $ map (C.opaque . FC.colourToDataColour)
        [ FC.FutuAccent FC.AF4 FC.AC3
        , FC.FutuAccent FC.AF1 FC.AC3
        , FC.FutuAccent FC.AF2 FC.AC3
        , FC.FutuAccent FC.AF3 FC.AC3
        -- , FC.FutuAccent FC.AF4 FC.AC2
        , FC.FutuGreen
        ]

    C.plot $ C.line "average"        [map (fmap (view _1)) xs]
    C.plot $ C.line "p=25"           [map (fmap (view _3)) xs]
    C.plot $ C.line "p=50,median"    [map (fmap (view _4)) xs]
    C.plot $ C.line "p=75"           [map (fmap (view _5)) xs]
    -- C.plot $ C.line "p=100, maximum" [map (fmap (view _2)) xs]
    C.plot $ C.line "n (non zero)"   [map (fmap (view _6)) xs]
  where
    days = [ addDays (-360) today .. today ]

    xs :: [(Day, (Double, Double, Double, Double, Double, Double))]
    xs = days <&> \d ->
        let -- is' = active ! d
            bs = sort
                [ b ! d
                | se <- ses ! d
                , P.employeeIsActive d se
                , (e, b) <- balances ^.. ix (se ^. P.employeeId)
                , e ^. P.employeeEmploymentType == Just P.Internal
                ]

            n = length bs
            s = sum bs
            m = maximum bs
            a = if n > 0 then s / fromIntegral n else 0
            p25 = if n > 0 then bs !! (n `div` 4) else 0
            p50 = if n > 0 then bs !! (n `div` 2) else 0
            p75 = if n > 0 then bs !! (3 * n `div` 4) else 0
        in (d, (a, m, p25, p50, p75, fromIntegral n))

-------------------------------------------------------------------------------
-- Render
-------------------------------------------------------------------------------

instance ToHtml InventorySummary where
    toHtmlRaw = toHtml
    toHtml    = toHtml . inventorySummaryRender

inventorySummaryRender :: InventorySummary -> HtmlPage "inventory-accounts"
inventorySummaryRender (InventorySummary today _ balances) = page_ "Inventory accounts" $ fullRow_ $ do
    let xs = Map.filter (\(e, _) -> P.employeeIsActive today e && e ^. P.employeeEmploymentType == Just P.Internal)
           $ fmap (fmap (! today)) balances

    h1_ "Inventory Summary"

    let s = sum (fmap snd xs)
    let m = maximum (fmap snd xs)

    h2_ "Metrics"
    dl_ $ do

        dt_ "Sum"
        dl_ $ toHtml $ show s

        dt_ "Max"
        dl_ $ toHtml $ show m

        dt_ "Average"
        dl_ $ toHtml $ show $ s / fromIntegral (length xs)

        dt_ "Num with maxed balance"
        dl_ $ toHtml $ show $ length $ filter ((>= m) . snd) (toList xs)

        dt_ "Num with > 1000"
        dl_ $ toHtml $ show $ length $ filter ((>= 1000) . snd) (toList xs)

        dt_ "Num with > 600"
        dl_ $ toHtml $ show $ length $ filter ((>= 600) . snd) (toList xs)

    h2_ "Average and quantiles over time"
    img_ [ src_ "/charts/inventory-quantiles", width_ "1000" ]

    h2_ "Accounts"
    table_ $ do
        thead_ $ tr_ $ do
            th_ "Name"
            th_ "Balance"

        tbody_ $ for_ (sortOn (Down . snd) $ toList xs) $ \(e, bal) -> tr_ $ do
            td_ $ toHtml $ e ^. P.employeeFullname
            td_ $ toHtml $ show bal

