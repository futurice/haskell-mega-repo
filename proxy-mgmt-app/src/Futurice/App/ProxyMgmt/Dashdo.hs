{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.ProxyMgmt.Dashdo (
    makeDashdoServer,
    ) where

import Control.Lens          ((<&>))
import Dashdo.Elements
import Dashdo.Rdash          (rdash)
import Dashdo.Rdash          (charts)
import Dashdo.Servant        (DashdoAPI, dashdoServer)
import Dashdo.Types
import Data.Aeson            (toJSON)
import Data.Ord              (Down (..))
import Futurice.Postgres     (safePoolQuery_)
import Futurice.Prelude
import Futurice.Servant      (cachedIO)
import Graphics.Plotly.Lucid (plotlyCDN)
import Lucid                 hiding (for_)
import Lucid.Bootstrap
import Lucid.Bootstrap3
import Prelude ()
import Servant               (Server)

import qualified Data.Map                   as Map
import qualified Database.PostgreSQL.Simple as Postgres
import qualified Graphics.Plotly            as Plotly

import Futurice.App.ProxyMgmt.Ctx

-------------------------------------------------------------------------------
-- Val
-------------------------------------------------------------------------------

data Val = Val
    { vUser     :: !Text
    , vStamp    :: !UTCTime
    , vEndpoint :: !Text
    }
  deriving (Generic)

instance NFData Val

fetchValues :: DashdoCtx -> IO [Val]
fetchValues DashdoCtx {..} =
    cachedIO dctxLogger dctxCache 600 () $ runLogT "fetchValues" dctxLogger $ do
        res <- safePoolQuery_ dctxPostgresPool
            "SELECT username, updated, endpoint FROM proxyapp.accesslog WHERE current_timestamp - updated < '1 month' :: interval ORDER BY updated DESC;"
        return $ fmap mkVal res

mkVal :: (Text, UTCTime, Text) -> Val
mkVal (a,b,c) = Val a b c

-------------------------------------------------------------------------------
-- Params
-------------------------------------------------------------------------------

data Params = Params
    { _pUsers     :: ![Text]
    , _pEndpoints :: ![Text]
    , _pDays      :: ![Text]
    }
  deriving Show

makeLenses ''Params

params0 :: Params
params0 = Params [] [] []

-------------------------------------------------------------------------------
-- Charts
-------------------------------------------------------------------------------

usersPie :: [Val] -> SHtml IO Params ()
usersPie vs = plotlySelectMultiple plotly pUsers
  where
    plotly = Plotly.plotly "users" [trace]

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

    m :: Map Text Double
    m = Map.fromListWith (+) $
        map (\Val {..} -> (vUser, 1)) vs

    l :: [(Text, Double)]
    l = sortOn (Down . snd) $ Map.toList m

    pieLabels :: [Text]
    pieLabels = fst <$> l

    pieValues :: [Double]
    pieValues = snd <$> l

    pieColors = Plotly.List $ toJSON <$> zipWith const colors pieLabels

endpointsPie :: [Val] -> SHtml IO Params ()
endpointsPie vs = plotlySelectMultiple plotly pEndpoints
  where
    plotly = Plotly.plotly "endpoints" [trace]

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

    m :: Map Text Double
    m = Map.fromListWith (+) $
        map (\Val {..} -> (vEndpoint, 1)) vs

    l :: [(Text, Double)]
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
    trace = Plotly.vbarChart $ map (first textShow) $ sortOn snd $ Map.toList m

    m :: Map Day Double
    m = Map.fromListWith (+) $ vs <&> \Val {..} ->
        (utctDay vStamp, 1)

-------------------------------------------------------------------------------
-- Colors
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Table
-------------------------------------------------------------------------------

valueTable :: [Val] -> SHtml IO Params ()
valueTable vs = table_ [ class_ "table table-striped" ] $ do
    thead_ $ tr_ $ do
        th_ "User"
        th_ "Stamp"
        th_ "Endpoint"

    -- only 10000 first requests
    -- TODO make a checkbox to fetch all
    tbody_ $ for_ (take 10000 vs) $ \Val {..} -> tr_ $ do
        td_ $ toHtml vUser
        td_ $ toHtml $ show vStamp
        td_ $ toHtml vEndpoint

-------------------------------------------------------------------------------
-- Usage dashboard
-------------------------------------------------------------------------------

proxyRDashdo :: DashdoCtx -> RDashdo IO
proxyRDashdo ctx =
    RDashdo "usage" "Prox usage" $ Dashdo params0 $ usage ctx

usage :: DashdoCtx -> SHtml IO Params ()
usage ctx = do
    -- parameters and values
    values <- liftIO $ fetchValues ctx
    params <- getValue

    -- filters
    let userFilter = case _pUsers params of
            [] -> id
            ts -> filter ((`elem` ts) . vUser)

    let endpointFilter = case _pEndpoints params of
            [] -> id
            ts -> filter ((`elem` ts) . vEndpoint)

    let dayFilter = case _pDays params of
            [] -> id
            ts -> filter ((`elem` ts) . textShow . utctDay . vStamp)

    -- filtered values
    let valuesNoUsers     = dayFilter . endpointFilter  $ values
    let valuesNoEndpoints = dayFilter .  userFilter     $ values
    let valuesNoDays      = endpointFilter . userFilter $ values

    let filteredValues    = userFilter valuesNoUsers

    -- charts
    charts
        [ ("Requests per user",     usersPie valuesNoUsers)
        , ("Requests per endpoint", endpointsPie valuesNoEndpoints)
        ]

    charts
        [ ("Requests per day", dayBarPlot valuesNoDays)
        ]

    -- table
    row_ $ mkCol [(MD,12)] $ do
        valueTable filteredValues

-------------------------------------------------------------------------------
-- Server
-------------------------------------------------------------------------------

makeDashdoServer :: DashdoCtx -> IO (Server DashdoAPI)
makeDashdoServer ctx = do
    let html = rdash dashdos plotlyCDN
    dashdoServer id html dashdos
  where
    dashdos =
        [ proxyRDashdo ctx
        ]

{-
accessReport :: Ctx -> IO AccessReport
accessReport Ctx { ctxPostgresPool = pool } = do
    now <- getCurrentTime
    withResource pool $ \conn -> do
        res <- Postgres.query_ conn
            "SELECT username, updated, endpoint FROM proxyapp.accesslog ORDER BY updated DESC LIMIT 1000;"
        return $ Report (ReportGenerated now) $ V.fromList $ map toNP res
  where
    toNP (a, b, c) = I a :* I b :* I c :* Nil

usersReport :: Ctx -> IO UsersReport
usersReport Ctx { ctxPostgresPool = pool } = do
    now <- getCurrentTime
    withResource pool $ \conn -> do
        res <- Postgres.query_ conn
            "SELECT username, createdby, createdat FROM proxyapp.credentials;"
        return $ Report (ReportGenerated now) $ V.fromList $ map toNP res
  where
    toNP (a, b, c) = I a :* I b :* I c :* Nil
-}
