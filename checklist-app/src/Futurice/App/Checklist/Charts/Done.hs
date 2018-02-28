{-# LANGUAGE DataKinds #-}
module Futurice.App.Checklist.Charts.Done (doneChart) where

import Control.Lens     (ifoldMapOf, (.=))
import Futurice.Prelude
import Prelude ()
import Servant.Chart    (Chart (..))

import qualified Graphics.Rendering.Chart.Easy as C

import Futurice.App.Checklist.Types

doneChart
    :: World       -- ^ the world
    -> Day         -- ^ today
    -> AuthUser
    -> Chart "done"
doneChart world _today _ = Chart . C.toRenderable $ do
    C.layout_title .= "tasks done per employee"
    C.layout_x_axis . C.laxis_title .= "due day"
    C.layout_y_axis . C.laxis_title .= "done %"

    C.plot $ pure $ C.PlotPoints
        { C._plot_points_title = "All"
        , C._plot_points_style = C.def
            & C.point_color        .~ C.opaque C.black
            & C.point_border_color .~ C.opaque C.black
            & C.point_border_width .~ 1
            & C.point_radius       .~ 4
            & C.point_shape        .~ C.PointShapePlus
        , C._plot_points_values = mkAllPoint <$> rawPoints
        }

    for_ [minBound .. maxBound] $ \role ->
        C.plot $ pure $ C.PlotPoints
            { C._plot_points_title = taskRoleToText role ^. unpacked
            , C._plot_points_style = C.def
                & C.point_color        .~ C.opaque (colors ^. ix role)
                & C.point_border_color .~ C.opaque (colors ^. ix role)
                & C.point_border_width .~ 1
                & C.point_radius       .~ 4
                & C.point_shape        .~ C.PointShapePlus
            , C._plot_points_values = mkViewerPoint role <$> rawPoints
            }
 where
    colors = PerTaskRole C.blue C.green C.red

    rawPoints :: [(Day, TodoCounter)]
    rawPoints =
        (world ^.. worldEmployees . folded . getter mkPoint)
        ++ (world ^.. worldArchive . folded . getter mkArchivedPoint)

    mkAllPoint :: (Day, TodoCounter) -> (Day, C.Percent)
    mkAllPoint (d, TodoCounter (Counter i j) _) =
        ( d
        , C.Percent $ 100 * fromIntegral i / fromIntegral j
        )

    mkViewerPoint :: TaskRole -> (Day, TodoCounter) -> (Day, C.Percent)
    mkViewerPoint r (d, TodoCounter _ perRole) = case perRole ^. ix r of
        Counter i j ->
            ( d
            , C.Percent $ 100 * fromIntegral i / fromIntegral j
            )

    mkPoint :: Employee -> (Day, TodoCounter)
    mkPoint e = (startingDay, counter)
      where
        eid = e ^. identifier
        startingDay = e ^. employeeStartingDay
        counter = ifoldMapOf
            (worldTaskItems . ix eid . ifolded)
            (toTodoCounter world)
            world

    mkArchivedPoint :: ArchivedEmployee -> (Day, TodoCounter)
    mkArchivedPoint (ArchivedEmployee e m) = (e ^. employeeStartingDay, mapToCounter m)
      where mapToCounter :: Map (Identifier Task) TaskItem -> TodoCounter
            mapToCounter = ifoldMap (taskItemtoTodoCounter world)
