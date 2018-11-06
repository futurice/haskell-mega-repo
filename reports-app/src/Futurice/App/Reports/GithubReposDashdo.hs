{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Reports.GithubReposDashdo (githubReposRDashdo) where

import Control.Lens          (ALens', cloneLens)
import Dashdo.Elements
import Dashdo.Rdash          (charts)
import Dashdo.Types
import Data.Aeson            (toJSON)
import Data.Time             (diffUTCTime)
import Data.Vec.Lazy         (Vec (..), mapWithVec)
import Futurice.Integrations
import Futurice.Prelude
import Futurice.Servant      (Cache, cachedIO)
import Lucid                 hiding (for_)
import Lucid.Bootstrap
import Lucid.Bootstrap3
import Prelude ()

import qualified Data.Map        as Map
import qualified GitHub          as GH
import qualified Graphics.Plotly as Plotly

import Futurice.App.Reports.Config

-------------------------------------------------------------------------------
-- Age
-------------------------------------------------------------------------------

data Age
    = AgeTwoWeek
    | AgeTwoMonth
    | AgeSixMonth
    | AgeOneYear
    | AgeTwoYear
    | AgeOlder
  deriving (Eq, Ord, Enum, Bounded, Generic)

instance NFData Age

ageToText :: Age -> Text
ageToText AgeTwoWeek  = "< 2 weeks"
ageToText AgeTwoMonth = "< 2 months"
ageToText AgeSixMonth = "< 6 months"
ageToText AgeOneYear  = "< 1 year"
ageToText AgeTwoYear  = "< 2 years"
ageToText AgeOlder    = "over 2 years"

ndtToAge :: NominalDiffTime -> Age
ndtToAge a
    | a < 86400 * 14  = AgeTwoWeek
    | a < 86400 * 60  = AgeTwoMonth
    | a < 86400 * 180 = AgeSixMonth
    | a < 86400 * 365 = AgeOneYear
    | a < 86400 * 730 = AgeTwoYear
    | otherwise       = AgeOlder

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

type Ctx = (Cache, Manager, Logger, Config)

data Val = Val
    { vOrgName   :: !Text
    , vRepoName  :: !Text
    , vUpdatedAt :: !(Maybe UTCTime)
    , vAge       :: !Age
    , vPrivate   :: !Bool
    , vArchived  :: !Bool
    }
  deriving (Generic)

instance NFData Val

fetchValues :: Ctx -> IO [Val]
fetchValues ctx = cachedIO' ctx () $
    runIntegrations' ctx action
  where
    action = do
        now <- currentTime
        orgName <- view githubOrganisationName

        repos <- githubReq $ GH.organizationReposR orgName GH.RepoPublicityAll GH.FetchAll

        return $ map (toVal now) $ sortOn GH.repoName $ toList repos

    toVal now r = Val
        { vOrgName   = GH.untagName $ GH.simpleOwnerLogin $ GH.repoOwner r
        , vRepoName  = GH.untagName $ GH.repoName r
        , vUpdatedAt = GH.repoUpdatedAt r
        , vAge       = maybe AgeTwoWeek (ndtToAge . diffUTCTime now) (GH.repoUpdatedAt r)
        , vPrivate   = GH.repoPrivate r
        , vArchived  = GH.repoArchived r
        }

runIntegrations' :: Ctx -> Integrations ReportIntegrations a -> IO a
runIntegrations' (_, mgr, lgr, cfg) m = do
    now <- currentTime
    runIntegrations mgr lgr now (cfgIntegrationsCfg cfg) m

cachedIO' :: (Eq k, Hashable k, Typeable k, NFData v, Typeable v) => Ctx -> k -> IO v -> IO v
cachedIO' (cache, _, logger, _) = cachedIO logger cache 600

-------------------------------------------------------------------------------
-- Params
-------------------------------------------------------------------------------

data Params = Params
    { _pVisibility :: [Text]
    , _pArchived   :: [Text]
    , _pAge        :: [Text]
    }
  deriving Show

makeLenses ''Params

params0 :: Params
params0 = Params [] [] []

-------------------------------------------------------------------------------
-- Parts
-------------------------------------------------------------------------------

data PlotlyPieOpts val params g m = PlotlyPieOpts
    { ppoName   :: Text
    , ppoToText :: g -> Text
    , ppoToPair :: val -> (g, Double)
    , ppoParam  :: ALens' params [Text]
    , ppoMetric :: (g, Double) -> m
    }

plotlyPie
    :: forall val params g m. (Ord g, Ord m)
    => PlotlyPieOpts val params g m -> [val] -> SHtml IO params ()
plotlyPie PlotlyPieOpts {..} vs = plotlySelectMultiple plotly' (cloneLens ppoParam)
  where
    plotly' = Plotly.plotly ppoName [trace]

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

    m :: Map g Double
    m = Map.fromListWith (+) $ map ppoToPair vs

    l :: [(g, Double)]
    l = sortOn ppoMetric $ Map.toList m

    pieLabels :: [Text]
    pieLabels = ppoToText . fst <$> l

    pieValues :: [Double]
    pieValues = snd <$> l

    pieColors = Plotly.List $ toJSON <$> zipWith const colors pieLabels

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

visibilityPie :: [Val] -> SHtml IO Params ()
visibilityPie = plotlyPie PlotlyPieOpts
    { ppoName   = "visibility"
    , ppoParam  = pVisibility
    , ppoToText = visibilityToText
    , ppoToPair = \Val {..} -> (vPrivate, 1)
    , ppoMetric = fst
    }
  where
    visibilityToText True  = "private"
    visibilityToText False = "public"

archivedPie :: [Val] -> SHtml IO Params ()
archivedPie = plotlyPie PlotlyPieOpts
    { ppoName   = "archived"
    , ppoParam  = pArchived
    , ppoToText = archivedToText
    , ppoToPair = \Val {..} -> (vArchived, 1)
    , ppoMetric = fst
    }
  where
    archivedToText True  = "archived"
    archivedToText False = "normal"

agePie :: [Val] -> SHtml IO Params ()
agePie = plotlyPie PlotlyPieOpts
    { ppoName   = "ages"
    , ppoParam  = pAge
    , ppoToText = ageToText
    , ppoToPair = \Val {..} -> (vAge, 1)
    , ppoMetric = fst
    }

valueTable :: [Val] -> SHtml IO Params ()
valueTable vs = do
    ul_ $
      li_ $ toHtml $ show (length vs) <> " repositories"

    table_ [ class_ "table table-striped" ] $ do
        thead_ $ tr_ $ do
            th_ "Name"
            th_ "Update at"
            th_ "Private"
        tbody_ $ for_ vs $ \Val {..} -> tr_ $ do
            td_ $ a_ [ href_ $ "https://github.com/" <> vOrgName <> "/" <> vRepoName] $
                toHtml vRepoName
            td_ $ traverse_ (toHtml . show) vUpdatedAt
            td_ $
                if vPrivate
                then b_ "private"
                else i_ "public"

-------------------------------------------------------------------------------
-- Multi apply
-------------------------------------------------------------------------------

-- | Apply all other functions, then the one at the element.
--
-- >>> multiApply (1 :: Int) ((*2) ::: (*3) ::: (*5) ::: VNil)
-- 15 ::: 10 ::: 6 ::: VNil
--
-- >>> mapWithVec (multiApply (1 :: Int)) ((*2), (*3), (*5))
-- (15,10,6)
--
multiApply :: a -> Vec n (a -> a) -> Vec n a
multiApply _ VNil       = VNil
multiApply xs (f ::: fs) = case multiApply xs fs of
    ys -> appEndo (foldMap Endo fs) xs ::: fmap f ys

-------------------------------------------------------------------------------
-- Dashdo
-------------------------------------------------------------------------------

githubRepos :: Ctx -> SHtml IO Params ()
githubRepos ctx = do
    values <- liftIO $ fetchValues ctx
    params <- getValue

    -- filters
    let visibilityFilter = case _pVisibility params of
            [] -> id
            ts -> filter ((`elem` ts') . vPrivate)
              where
                ts' = mapMaybe f ts

                f "public"  = Just False
                f "private" = Just True
                f _         = Nothing

    let archivedFilter = case _pArchived params of
            [] -> id
            ts -> filter ((`elem` ts') . vPrivate)
              where
                ts' = mapMaybe f ts

                f "achived" = Just True
                f "normal"  = Just False
                f _         = Nothing

    let ageFilter = case _pAge params of
            [] -> id
            ts -> filter ((`elem` ts) . ageToText . vAge)

    -- values
    let (valuesNoVisibility, valuesNoArchived, valuesNoAge) =
          mapWithVec (multiApply values)
          (visibilityFilter, archivedFilter, ageFilter)

    let filteredValues = ageFilter valuesNoAge

    -- Top
    row_ $ mkCol [(MD,12)] $ do
        em_ "Note: the GitHub data is up to 24 hours old"

    -- charts
    charts
        [ ("Repositories by visibility", visibilityPie valuesNoVisibility)
        , ("Repositories by archived", archivedPie valuesNoArchived)
        , ("Repositories by age", agePie valuesNoAge)
        ]

    -- table
    row_ $ mkCol [(MD,12)] $ do
        valueTable filteredValues

githubReposRDashdo :: Ctx -> RDashdo IO
githubReposRDashdo ctx =
    RDashdo "github-repos" "Github Repositories" $
    Dashdo params0 $ githubRepos ctx
