{-# LANGUAGE ScopedTypeVariables #-}
module Futurice.Chart.Stacked where

import Control.Monad.State.Strict (State, evalState, state)
import Data.Functor.Rep
import Futurice.Prelude
import Prelude ()

import Graphics.Rendering.Chart.Easy hiding (index)

data PlotStacked f x y = PlotStacked
    { _plot_stacked_titles      :: f String

    , _plot_stacked_values      :: [(x, f y)]

    , _plot_stacked_fill_styles :: f FillStyle

    , _plot_stacked_line_styles :: f LineStyle
    }

stacked
    :: (Representable f, Traversable f)
    => f String -> [(x, f y)] -> EC l (PlotStacked f x y)
stacked titles values = do
    let units = pureRep ()
    styles <- for units $ \_ -> do
        c <- takeColor
        pure (solidFillStyle $ dissolve 0.3 c, solidLine 1.5 c)
    pure $ PlotStacked
        { _plot_stacked_titles      = titles
        , _plot_stacked_values      = values
        , _plot_stacked_fill_styles = fmap fst styles
        , _plot_stacked_line_styles = fmap snd styles
        }

emptyPlot :: Plot x y
emptyPlot = Plot
    { _plot_render     = \_ -> pure ()
    , _plot_legend     = []
    , _plot_all_points = ([], [])
    }

stackedToPlot
    :: forall f x y. (Traversable f, Representable f, Num y)
    => PlotStacked f x y -> Plot x y
stackedToPlot ps =
    foldr joinPlot emptyPlot (fmap mk (tabulate id :: f (Rep f)))
  where
    mk idx = joinPlot (fillPlot idx) (linePlot idx)

    titles = _plot_stacked_titles ps
    values = fmap2 (prefixSum 0) (_plot_stacked_values ps)
    lineStyles = _plot_stacked_line_styles ps
    fillStyles = _plot_stacked_fill_styles ps

    linePlot :: Rep f -> Plot x y
    linePlot idx = toPlot $ PlotLines
        { _plot_lines_title  = index titles idx
        , _plot_lines_style  = index lineStyles idx
        , _plot_lines_values = [fmap2 (snd . flip index idx) values]
        , _plot_lines_limit_values = []
        }

    fillPlot :: Rep f -> Plot x y
    fillPlot idx = toPlot $ PlotFillBetween
        { _plot_fillbetween_title  = index titles idx
        , _plot_fillbetween_style  = index fillStyles idx
        , _plot_fillbetween_values = fmap2 (flip index idx) values
        }

fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap

prefixSum :: forall f a. (Traversable f, Num a) => a -> f a -> f (a, a)
prefixSum i xs = evalState (traverse mk xs) i where
    mk :: a -> State a (a, a)
    mk = \x -> state $ \y -> let z = x + y in ((y, z), z)
