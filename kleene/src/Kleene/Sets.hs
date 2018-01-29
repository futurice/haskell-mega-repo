-- | Character sets.
module Kleene.Sets (
    dotRSet,
    rsetToJS,
    ) where

import Data.Monoid        (Endo (..))
import Data.RangeSet.List (RSet)

import qualified Data.RangeSet.List as RSet

dotRSet :: RSet Char
dotRSet = RSet.full RSet.\\ RSet.singleton '\n'

-- TODO: escaping, ranges
rsetToJS :: RSet Char -> ShowS
rsetToJS cs
    = showChar '['
    . appEndo (foldMap (Endo . f) (RSet.toRangeList cs))
    . showChar ']'
  where
    f (a, b)
      | a == b = showChar a
      | otherwise = showChar a . showChar '-' . showChar b
