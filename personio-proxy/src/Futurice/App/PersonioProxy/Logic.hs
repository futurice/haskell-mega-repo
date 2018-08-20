{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.PersonioProxy.Logic where

import Control.Concurrent.STM           (readTVarIO)
import Control.Lens                     ((.=))
import Data.Aeson.Types                 (parseEither, parseJSON)
import Data.Distributive                (Distributive (..))
import Data.Functor.Rep                 (Representable (..), distributeRep, pureRep, apRep)
import Data.List                        (partition, foldl')
import Futurice.App.PersonioProxy.Types
import Futurice.Cache                   (cached)
import Futurice.Prelude
import Futurice.Tribe
import Prelude ()
import Servant
import Servant.Chart                    (Chart (..))

import qualified Futurice.Colour as FC
import qualified Data.Map.Strict               as Map
import qualified Futurice.Chart.Stacked        as C
import qualified Futurice.Postgres             as Postgres
import qualified Graphics.Rendering.Chart.Easy as C
import qualified Personio                      as P

personioRequest :: Ctx -> P.SomePersonioReq -> Handler P.SomePersonioRes
personioRequest ctx (P.SomePersonioReq res) = case res of
    P.PersonioEmployees   -> P.SomePersonioRes res <$> rawEmployees ctx
    P.PersonioValidations -> P.SomePersonioRes res <$> rawValidations ctx
    P.PersonioAll         -> do
        es <- rawEmployees ctx
        vs <- rawValidations ctx
        pure (P.SomePersonioRes res (es, vs))

rawValidations :: Ctx -> Handler [P.EmployeeValidation]
rawValidations ctx = liftIO $ readTVarIO $ ctxPersonioValidations ctx

rawEmployees :: Ctx -> Handler [P.Employee]
rawEmployees ctx = do
    es <- liftIO $ readTVarIO $ ctxPersonio ctx
    -- no filtering, all employees
    pure $ toList es

scheduleEmployees :: Ctx -> Handler [P.ScheduleEmployee]
scheduleEmployees ctx = do
    es <- liftIO $ readTVarIO $ ctxPersonio ctx
    -- no filtering, all employees
    pure $ P.fromPersonio $ toList es

-------------------------------------------------------------------------------
-- Employees Chart
-------------------------------------------------------------------------------

employeesChart :: Ctx -> Handler (Chart "employees")
employeesChart ctx = do
    values <- liftIO $ runLogT "chart-employees" (ctxLogger ctx) $
        cached (ctxCache ctx) 600 () $ do
            res <- Postgres.safePoolQuery_ ctx "SELECT DISTINCT ON (timestamp :: date) timestamp, contents FROM \"personio-proxy\".log;"
            return $ res <&> \(t, c) ->
                let d = utctDay t
                in (d, employeesCounts d c)

    return $ Chart . C.toRenderable $ do
        C.layout_title .= "Active employees"

        C.plot $ fmap C.stackedToPlot $ C.stacked
            (PerEmploymentType "Internal" "External")
            values

        C.layout_x_axis . C.laxis_title .= "day"
        C.layout_y_axis . C.laxis_title .= "Employees"
        C.layout_y_axis . C.laxis_override .= overrideY
  where
    employeesCounts :: Day -> Value -> PerEmploymentType Int
    employeesCounts today v = case parseEither parseJSON v of
        Left _    -> PerEmploymentType 0 0
        -- TODO: use @folds@ -package
        Right es' -> PerEmploymentType (length int) (length ext)
          where
            es = filter (P.employeeIsActive today) es'
            (int, ext) = partition (\p -> p ^. P.employeeEmploymentType /= Just P.External) es

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

tribeEmployeesChart :: Ctx -> Handler (Chart "tribe-employees")
tribeEmployeesChart ctx = do
    values <- liftIO $ runLogT "chart-tribe-employees" (ctxLogger ctx) $
        cached (ctxCache ctx) 600 () $ do
            res <- Postgres.safePoolQuery_ ctx "SELECT DISTINCT ON (timestamp :: date) timestamp, contents FROM \"personio-proxy\".log;"
            return $ res <&> \(t, c) ->
                let d = utctDay t
                in (d, employeesCounts d c)

    return $ Chart . C.toRenderable $ do
        C.setColors $ cycle
            [ C.opaque $ FC.colourToDataColour $ FC.FutuAccent f a
            | f <- [ minBound .. maxBound ]
            , a <- [ FC.AC2, FC.AC3 ]
            ]
        
        C.layout_title .= "Active employees per tribe"

        C.plot $ fmap C.stackedToPlot $ C.stacked
            (tabulate $ view unpacked . tribeToText)
            values

        C.layout_x_axis . C.laxis_title .= "day"
        C.layout_y_axis . C.laxis_title .= "Employees"
        C.layout_y_axis . C.laxis_override .= overrideY
  where
    employeesCounts :: Day -> Value -> PerTribe Int
    employeesCounts today v = case parseEither parseJSON v of
        Left _    -> pureRep 0
        Right es' -> foldl' go (pureRep 0) es
          where
            es :: [P.Employee]
            es = filter (\e -> P.employeeIsActive today e && e ^. P.employeeEmploymentType == Just P.Internal) es'

            go :: PerTribe Int -> P.Employee -> PerTribe Int
            go (Per m) e = Per $ m
                & at (e ^. P.employeeTribe) %~ Just . maybe 1 succ

    overrideY :: C.AxisData Int -> C.AxisData Int
    overrideY ax = ax & C.axis_grid .~ (ax ^.. C.axis_ticks . folded . _1)

-------------------------------------------------------------------------------
-- Roles distribution
-------------------------------------------------------------------------------

rolesDistributionChart :: Ctx -> Handler (Chart "roles-distribution")
rolesDistributionChart ctx = do
    values' <- liftIO $ runLogT "chart-roles-distribution" (ctxLogger ctx) $
        cached (ctxCache ctx) 600 () $ do
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
