{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
module Futurice.App.Reports.CareerLengthChart (
    careerLengthData,
    careerLengthRender,
    careerLengthRelativeRender,
    ) where

import Control.Lens          (mapped, (.=))
import Data.Time             (diffDays)
import Data.Vec.Lazy         (Vec (..))
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()
import Servant.Chart         (Chart (..))

import qualified Data.Type.Nat                 as N
import qualified Futurice.Chart.Stacked        as C
import qualified Graphics.Rendering.Chart.Easy as C
import qualified Personio                      as P

-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

type Buckets   = N.Nat7
type DataEntry = Vec Buckets Double
type DataSet   = [(Day, DataEntry)]

-------------------------------------------------------------------------------
-- Values
-------------------------------------------------------------------------------

careerLengthData
    :: forall m. ( MonadTime m, MonadPersonio m)
    => m DataSet
careerLengthData = do
    endDay <- currentDay
    let days = [ $(mkDay "2000-11-01"),  $(mkDay "2000-11-07") .. endDay ]

    employees <- P.personio P.PersonioEmployees

    pure $ map (\d -> (d, dataEntry d employees)) days

dataEntry :: Day -> [P.Employee] -> DataEntry
dataEntry d = fmap getSum . foldMap mk where
    mk :: P.Employee -> Vec Buckets (Sum Double)
    -- don't count externals and fixed term people
    mk p | p ^. P.employeeEmploymentType == Just P.External = pure 0
    mk p | p ^. P.employeeContractType == Just P.FixedTerm  = pure 0

    -- rest is what we intersted atm.
    mk p = case p ^. P.employeeHireDate of
        -- no hire date: drop
        Nothing -> pure 0
        -- not yet hired
        Just a
            | d < a     -> pure 0
            | otherwise -> case p ^. P.employeeEndDate of
                -- already left
                Just b | b < d -> pure 0
                -- between a and optional b
                _ -> fromDiff (diffDays d a)

    fromDiff :: Integer -> Vec Buckets (Sum Double)
    fromDiff x
        | x < 180   = 1 ::: 0 ::: 0 ::: 0 ::: 0 ::: 0 ::: 0 ::: VNil
        | y < 1     = 0 ::: 1 ::: 0 ::: 0 ::: 0 ::: 0 ::: 0 ::: VNil
        | y < 2     = 0 ::: 0 ::: 1 ::: 0 ::: 0 ::: 0 ::: 0 ::: VNil
        | y < 3     = 0 ::: 0 ::: 0 ::: 1 ::: 0 ::: 0 ::: 0 ::: VNil
        | y < 5     = 0 ::: 0 ::: 0 ::: 0 ::: 1 ::: 0 ::: 0 ::: VNil
        | y < 10    = 0 ::: 0 ::: 0 ::: 0 ::: 0 ::: 1 ::: 0 ::: VNil
        | otherwise = 0 ::: 0 ::: 0 ::: 0 ::: 0 ::: 0 ::: 1 ::: VNil
      where
        y = x `div` 365

relative :: (Traversable f, Applicative f) => f Double -> f Double
relative xs 
    | s < 1e-6  = pure 0
    | otherwise = fmap (\x -> x/s * 100) xs
  where
    s = sum xs
    

-------------------------------------------------------------------------------
-- Render
-------------------------------------------------------------------------------

careerLengthRender :: DataSet -> Chart "career-length"
careerLengthRender values = Chart . C.toRenderable $ do
    C.layout_title .= "Distribution of career length over time"
    C.layout_x_axis . C.laxis_title .= "day"
    C.layout_y_axis . C.laxis_title .= "people"

    C.plot $ fmap C.stackedToPlot $ C.stacked legends values

careerLengthRelativeRender :: DataSet -> Chart "career-length-relative"
careerLengthRelativeRender values = Chart . C.toRenderable $ do
    C.layout_title .= "Distribution of career length over time"
    C.layout_x_axis . C.laxis_title .= "day"
    C.layout_y_axis . C.laxis_title .= "percentage of personnel"

    C.plot $ fmap C.stackedToPlot $ C.stacked legends $ over (mapped . _2) relative values

legends :: Vec Buckets String
legends =
    "< 6 months" :::
    "6 - 12 months" :::
    "1 - 2 years" :::
    "2 - 3 years" :::
    "3 - 5 years" :::
    "5 - 10 years" :::
    "> 10 years" :::
    VNil
