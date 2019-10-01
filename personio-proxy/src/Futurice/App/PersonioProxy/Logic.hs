{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.PersonioProxy.Logic where

import Control.Concurrent.STM           (readTVarIO)
import Control.Lens                     ((.=))
import Control.Monad.State.Strict       (evalState, state)
import Data.Aeson.Types                 (parseEither, parseJSON)
import Data.Distributive                (Distributive (..))
import Data.Functor.Rep
       (Representable (..), apRep, distributeRep, pureRep)
import Data.List                        (foldl', partition)
import Data.Time                        (addDays)
import Futurice.App.PersonioProxy.Types
import Futurice.Cache                   (cachedIO)
import Futurice.CareerLevel
import Futurice.Daily
import Futurice.Prelude
import Futurice.Tribe
import Prelude ()
import Servant
import Servant.Cached                   (Cached, mkCached)
import Servant.Chart                    (Chart (..), SVG)

import qualified Data.Map.Strict               as Map
import qualified Futurice.Chart.Stacked        as C
import qualified Futurice.Colour               as FC
import qualified Futurice.IdMap                as IdMap
import qualified Futurice.Postgres             as Postgres
import qualified Graphics.Rendering.Chart.Easy as C
import qualified Personio                      as P

personioRequest :: Ctx -> P.SomePersonioReq -> Handler P.SomePersonioRes
personioRequest ctx (P.SomePersonioReq res) = case res of
    P.PersonioEmployees       -> P.SomePersonioRes res <$> rawEmployees ctx
    P.PersonioValidations     -> P.SomePersonioRes res <$> rawValidations ctx
    P.PersonioSimpleEmployees -> P.SomePersonioRes res <$> rawSimpleEmployees ctx
    P.PersonioAll             -> P.SomePersonioRes res <$> rawAllData ctx

rawValidations :: Ctx -> Handler [P.EmployeeValidation]
rawValidations ctx = P.paValidations <$> liftIO (readTVarIO $ ctxPersonioData ctx)

rawEmployees :: Ctx -> Handler [P.Employee]
rawEmployees ctx = do
    es <- P.paEmployees <$> (liftIO $ readTVarIO $ ctxPersonioData ctx)
    -- no filtering, all employees
    pure $ toList es

rawAllData :: Ctx -> Handler P.PersonioAllData
rawAllData ctx = liftIO $ readTVarIO $ ctxPersonioData ctx

scheduleEmployees :: Ctx -> Handler [P.ScheduleEmployee]
scheduleEmployees ctx = do
    es <- P.paEmployees <$> (liftIO $ readTVarIO $ ctxPersonioData ctx)
    -- no filtering, all employees
    pure $ P.fromPersonio $ toList es

getSimpleEmployees :: Ctx -> LogT IO (Map Day [P.SimpleEmployee])
getSimpleEmployees ctx = do
    res <- Postgres.safePoolQuery_ ctx "SELECT DISTINCT ON (timestamp :: date) timestamp, contents FROM \"personio-proxy\".log;"
    let ses' :: Map Day [P.SimpleEmployee]
        ses' = P.internSimpleEmployees (traverse . traverse) $ Map.fromList
            [ (d, ids)
            | (t, v) <- res
            , let d = utctDay t
            , let ids = case parseEither parseJSON v of
                    Left _   -> mempty
                    Right es -> (es :: [P.Employee]) ^.. folded . P.simpleEmployee
            ]
    pure ses'

--needed so rarely that we can just fetch stuff on need basis
rawSimpleEmployees :: Ctx -> Handler (Map Day [P.SimpleEmployee])
rawSimpleEmployees ctx = liftIO $ runLogT "personio-proxy" (ctxLogger ctx) $ getSimpleEmployees ctx

-------------------------------------------------------------------------------
-- Cached
-------------------------------------------------------------------------------

mkCached' :: (Typeable ct, Typeable a, MimeRender ct a) => Ctx -> IO a -> Handler (Cached ct a)
mkCached' ctx action = liftIO $
    cachedIO (ctxLogger ctx) (ctxCache ctx) 600 () $ mkCached <$> action

-------------------------------------------------------------------------------
-- Employees Chart
-------------------------------------------------------------------------------

employeesChart :: Ctx -> Handler (Cached SVG (Chart "employees"))
employeesChart ctx = mkCached' ctx $ runLogT "chart-employees" (ctxLogger ctx) $ do
    today <- currentDay
    let ses = getSimpleEmployees ctx

    values <- do
        fes <- liftIO $ readTVarIO $ ctxPersonioData ctx
        let fes' = IdMap.fromFoldable $ P.paEmployees fes
        res <- fmap (dailyFromMap' []) ses
        return
            [ pair d $ employeesCounts fes' d (res ! d)
            | d <- [ $(mkDay "2018-06-01") .. today ]
            ]

    return $ Chart . C.toRenderable $ do
        C.layout_title .= "Employees"

        C.plot $ fmap C.stackedToPlot $ C.stacked
            (PerEmploymentType "Internal" "External")
            values

        C.layout_x_axis . C.laxis_title .= "day"
        C.layout_y_axis . C.laxis_title .= "Employees"
        C.layout_y_axis . C.laxis_override .= overrideY
  where
    pair :: a -> b -> (a, b)
    pair = (,)

    employeesCounts :: IdMap.IdMap P.Employee -> Day -> [P.SimpleEmployee] -> PerEmploymentType Int
    employeesCounts fes today ses' = PerEmploymentType (length int) (length ext)
      where
        ses = filter (P.employeeIsActive today) ses'
        (int, ext) = partition
            (\p -> fes ^? ix (p ^. P.employeeId) . P.employeeEmploymentType . _Just /= Just P.External)
            ses

    overrideY :: C.AxisData Int -> C.AxisData Int
    overrideY ax = ax & C.axis_grid .~ (ax ^.. C.axis_ticks . folded . _1)

data PerEmploymentType a = PerEmploymentType a a
  deriving (Functor, Foldable, Traversable, Generic)

instance NFData a => NFData (PerEmploymentType a)

instance Distributive PerEmploymentType where
    distribute = distributeRep

instance Applicative PerEmploymentType where
    pure  = pureRep
    (<*>) = apRep

instance Representable PerEmploymentType where
    type Rep PerEmploymentType = P.EmploymentType

    tabulate f = PerEmploymentType (f P.Internal) (f P.External)
    index (PerEmploymentType x _) P.Internal = x
    index (PerEmploymentType _ x) P.External = x

-------------------------------------------------------------------------------
-- Tribes employees chart
-------------------------------------------------------------------------------

tribeEmployeesChart :: Ctx -> Handler (Cached SVG (Chart "tribe-employees"))
tribeEmployeesChart ctx = mkCached' ctx $ runLogT "chart-tribe-employees" (ctxLogger ctx) $ do
    today <- currentDay
    let ses = getSimpleEmployees ctx

    (values, future) <- do
        fes <- liftIO $ readTVarIO $ ctxPersonioData ctx
        let fes' = IdMap.fromFoldable $ P.paEmployees fes
        res <- fmap (dailyFromMap' []) ses
        return $ pair
            [ pair d $ employeesCounts True fes' d (res ! d)
            | d <- [ $(mkDay "2018-06-01") .. today ]
            ]
            [ pair d $ employeesCounts False fes' d (res ! d)
            | d <- [ today .. addDays 50 today ]
            ]

    return $ Chart . C.toRenderable $ do
        C.layout_title .= "Employees per tribe"

        C.setColors $ cycle
            [ C.opaque $ FC.colourToDataColour $ FC.FutuAccent f a
            | f <- [ minBound .. maxBound ]
            , a <- [ FC.AC2, FC.AC3 ]
            ]

        C.plot $ fmap C.stackedToPlot $ C.stacked
            (tabulate $ view unpacked . tribeToText)
            values

        C.setColors $ cycle
            [ flip C.withOpacity 0.5 $ FC.colourToDataColour $ FC.FutuAccent f a
            | f <- [ minBound .. maxBound ]
            , a <- [ FC.AC2, FC.AC3 ]
            ]

        C.plot $ fmap C.stackedToPlot $ C.stacked
            (tabulate $ view unpacked . tribeToText)
            future

        C.layout_x_axis . C.laxis_title .= "day"
        C.layout_y_axis . C.laxis_title .= "Employees"
        C.layout_y_axis . C.laxis_override .= overrideY
  where
    employeesCounts :: Bool -> IdMap.IdMap P.Employee -> Day -> [P.SimpleEmployee] -> PerTribe Int
    employeesCounts status fes today es' = foldl' go (pureRep 0) es
      where
        es :: [P.SimpleEmployee]
        es = flip filter es' $ \e ->
            (if status then P.employeeIsActive else P.employeeIsActive') today e
            && fes ^? ix (e ^. P.employeeId) . P.employeeEmploymentType . _Just == Just P.Internal

        go :: PerTribe Int -> P.SimpleEmployee -> PerTribe Int
        go (Per m) e = Per $ m
            & at (e ^. P.employeeTribe) %~ Just . maybe 1 succ

    overrideY :: C.AxisData Int -> C.AxisData Int
    overrideY ax = ax & C.axis_grid .~ (ax ^.. C.axis_ticks . folded . _1)

    pair :: a -> b -> (a, b)
    pair = (,)

-------------------------------------------------------------------------------
-- Career levels chart
-------------------------------------------------------------------------------

careerLevelsChart :: Ctx -> Handler (Cached SVG (Chart "career-levels"))
careerLevelsChart ctx = mkCached' ctx $ runLogT "career-levels" (ctxLogger ctx) $ do
    (values, perRole) <- liftIO $ do
        xs <- liftIO $ readTVarIO $ ctxPersonioData ctx
        return (P.paCareerLevels xs, P.paCareerLevelsRole xs)

    let roles = Map.keys perRole

    let transp :: Map CareerLevel [Int]
        transp = Map.fromList
            [ (cl, xs)
            | cl <- [ minBound .. maxBound ]
            , let xs = fromMaybe 0 (values ^? ix cl)
                     : [ fromMaybe 0 $ perRole ^? ix r . ix cl | r <- roles ]
            ]

    let titles = "All" : fmap (view unpacked) roles
    let points :: [(CareerLevel, [Int])]
        points = Map.toList transp

    return $ Chart . C.toRenderable $ do
        C.layoutlr_title .= "Career level distribution"
        C.layoutlr_x_axis . C.laxis_title .= "Career level"
        C.layoutlr_left_axis . C.laxis_title .= "Employees"
        C.layoutlr_right_axis . C.laxis_title .= "Cumulative"

        colors
        C.plotLeft $ fmap (C.plotBars . style) $ C.bars titles points

        colors
        C.plotRight $ fmap C.toPlot $ C.line "All" [Map.toList $ sumScan values]
        ifor_ perRole $ \role xs ->
            C.plotRight $ fmap C.toPlot $ C.line (role ^. unpacked) [Map.toList $ sumScan xs]

  where
    colors =
        C.setColors $ cycle
            [ C.opaque $ FC.colourToDataColour c
            | c <-
                [ FC.FutuGreen
                , FC.FutuAccent FC.AF4 FC.AC3
                , FC.FutuAccent FC.AF1 FC.AC3
                , FC.FutuAccent FC.AF2 FC.AC3
                , FC.FutuAccent FC.AF3 FC.AC3
                ]
            ]

    style b = b
        & C.plot_bars_spacing .~ C.BarsFixWidth 5

    sumScan :: Traversable t => t Int -> t Int
    sumScan t = evalState (traverse (\x -> state $ \s -> let xs = s + x in xs `seq` (xs, xs)) t) 0

-------------------------------------------------------------------------------
-- Roles distribution
-------------------------------------------------------------------------------

rolesDistributionChart :: Ctx -> Handler (Cached SVG (Chart "roles-distribution"))
rolesDistributionChart ctx = mkCached' ctx $ runLogT "chart-roles-distribution" (ctxLogger ctx) $ do
    values' <- do
        res <- Postgres.safePoolQuery_ ctx "SELECT DISTINCT ON (timestamp :: date) timestamp, contents FROM \"personio-proxy\".log;"
        return $ res <&> \(t, c) ->
            let d = utctDay t
            in (d, rolesDistribution d c)

    -- titles are also positions!
    let (titles, values) = postprocess values'

    return $ Chart . C.toRenderable $ do
        C.setColors $ cycle
            [ C.opaque $ FC.colourToDataColour $ FC.FutuAccent f a
            | f <- [ minBound .. maxBound ]
            , a <- [ FC.AC2, FC.AC3 ]
            ]

        C.layout_title .= "Relative role/competence distribution over time"

        C.plot $ fmap (C.stackedToPlot' titles (flip indexTMap)) $ C.stacked
            (fmap (view unpacked) titles)
            values

        C.layout_x_axis . C.laxis_title .= "day"
        C.layout_y_axis . C.laxis_title .= "Percent"
        C.layout_y_axis . C.laxis_override .= overrideY
  where
    rolesDistribution :: Day -> Value -> TMap Text Int
    rolesDistribution today v = case parseEither parseJSON v of
        Left _    -> pure 0
        Right es' -> foldl' go (pure 0) es
          where
            es :: [P.Employee]
            es = filter (\e -> P.employeeIsActive today e && e ^. P.employeeEmploymentType == Just P.Internal) es'

            go :: TMap Text Int -> P.Employee -> TMap Text Int
            go (TMap d m) e = TMap d $ m
                & at (e ^. P.employeeRole) %~ Just . maybe 1 succ

    overrideY :: C.AxisData Double -> C.AxisData Double
    overrideY ax = ax
        & C.axis_grid   %~ filter (<= 100)
        & C.axis_ticks  %~ filter ((<= 100) . fst)
        & C.axis_labels %~ map (filter ((<= 100) . fst))

    postprocess :: [(Day, TMap Text Int)] -> (TMap Text Text, [(Day, TMap Text Double)])
    postprocess xs = (titles, values)
      where
        keys = mconcat [ Map.keysSet m | (_, TMap _ m) <- xs ]
        keys' = Map.fromSet (const 0) keys

        titles = TMap "?" $ Map.fromSet id keys
        values =
            [ (d, TMap 0 $ fmap (\x -> 100 * fromIntegral x / s) m')
            | (d, TMap _  m) <- xs
            , let m' = Map.union m keys'
            , let s = fromIntegral (sum m')
            ]


-------------------------------------------------------------------------------
-- Per Enum/Bounded
-------------------------------------------------------------------------------

type PerTribe = Per Tribe

-- | This might be useful elsewhere.
--
newtype Per k a = Per (Map k a)
  deriving (Functor, Foldable, Traversable, Generic)

instance (NFData k, NFData a) => NFData (Per k a)

instance (Ord k, Enum k, Bounded k) => Applicative (Per k) where
    pure  = pureRep
    (<*>) = apRep

instance (Ord k, Enum k, Bounded k) => Distributive (Per k) where
    distribute = distributeRep

instance (Ord k, Enum k, Bounded k) => Representable (Per k) where
    type Rep (Per k) = k

    tabulate f = Per $ Map.fromList
        [ (t, f t)
        | t <- [ minBound .. maxBound ]
        ]

    index (Per m) t = fromMaybe (error "panic! index @Per") $ m ^? ix t

-------------------------------------------------------------------------------
-- TMap
-------------------------------------------------------------------------------

data TMap k v = TMap v (Map k v)
  deriving (Functor, Foldable, Traversable, Generic)

instance (NFData k, NFData a) => NFData (TMap k a)

indexTMap :: Ord k => k -> TMap k v -> v
indexTMap k (TMap def m) = fromMaybe def (Map.lookup k m)

instance Ord k => Applicative (TMap k) where
    pure x = TMap x mempty

    TMap f fm <*> TMap x xm = TMap (f x) $ Map.unions
        [ Map.intersectionWith ($) fm xm
        , fmap f xm
        , fmap ($ x) fm
        ]
