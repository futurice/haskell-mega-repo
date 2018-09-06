module Futurice.Chart.Enum (
    C.PlotValue (..),
    enumToValue,
    enumFromValue,
    enumAutoAxis,
    ) where

import Futurice.Prelude
import Prelude ()

import qualified Graphics.Rendering.Chart.Easy as C

enumToValue :: Enum a => a -> Double
enumToValue = fromIntegral . fromEnum

enumFromValue :: forall a. (Enum a, Bounded a) => Double -> a
enumFromValue x = case round x of
    n | n < fromEnum (minBound :: a) -> minBound
      | n > fromEnum (maxBound :: a) -> maxBound
      | otherwise                    -> toEnum n

-- TODO: use enumToValue / enumFromValue in vmap/invmap
enumAutoAxis :: (Bounded a, Enum a, C.PlotValue a) => (a -> Text) -> [a] -> C.AxisData a
enumAutoAxis f _ = C.AxisData
    { C._axis_visibility = C.def
    , C._axis_viewport   = vmap (minBound, maxBound)
    , C._axis_tropweiv   = invmap (minBound, maxBound)
    , C._axis_ticks      = []
    , C._axis_grid       = []
    , C._axis_labels     = [[ (v, f v ^. unpacked) | v <- vals ]]
    }
  where
    vals = [minBound .. maxBound]

-- | A linear mapping of points in one range to another.
vmap :: C.PlotValue x => (x,x) -> C.Range -> x -> Double
vmap (v1,v2) (r1,r2) x
    = r1
    + (1 + C.toValue x - C.toValue v1)
    * (r2-r1)
    / range
  where
    range =  2 + C.toValue v2 - C.toValue v1

-- | The inverse mapping from device co-ordinate range back to
--   interesting values.
invmap :: C.PlotValue x => (x,x) -> C.Range -> Double -> x
invmap (v1,v2) (r1,r2) y = C.fromValue
    $ (C.toValue v1 - 1)
    + (y - r1)
    * range
    / (r2-r1)
  where
    range =  2 + C.toValue v2 - C.toValue v1
