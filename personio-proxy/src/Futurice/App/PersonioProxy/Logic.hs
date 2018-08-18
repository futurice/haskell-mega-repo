{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.PersonioProxy.Logic where

import Control.Concurrent.STM           (readTVarIO)
import Control.Lens                     ((.=))
import Data.Aeson.Types                 (parseEither, parseJSON)
import Data.Distributive                (Distributive (..))
import Data.Functor.Rep                 (Representable (..), distributeRep, pureRep)
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

instance Representable PerEmploymentType where
    type Rep PerEmploymentType = P.EmploymentType

    tabulate f = PerEmploymentType (f P.Internal) (f P.External)
    index (PerEmploymentType x _) P.Internal = x
    index (PerEmploymentType _ x) P.External = x

-------------------------------------------------------------------------------
-- Tribes emplpyees chart
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
            es = filter (P.employeeIsActive today) es'

            go :: PerTribe Int -> P.Employee -> PerTribe Int
            go (PerTribe m) e = PerTribe $ m
                & at (e ^. P.employeeTribe) %~ Just . maybe 1 succ

    overrideY :: C.AxisData Int -> C.AxisData Int
    overrideY ax = ax & C.axis_grid .~ (ax ^.. C.axis_ticks . folded . _1)

-- | This might be useful elsewhere.
--
newtype PerTribe a = PerTribe (Map Tribe a)
  deriving (Functor, Foldable, Traversable, Generic)

instance NFData a => NFData (PerTribe a)

instance Distributive PerTribe where
    distribute = distributeRep

instance Representable PerTribe where
    type Rep PerTribe = Tribe

    tabulate f = PerTribe $ Map.fromList
        [ (t, f t)
        | t <- [ minBound .. maxBound ]
        ]

    index (PerTribe m) t = fromMaybe (error "panic! index @PerTribe") $ m ^? ix t

