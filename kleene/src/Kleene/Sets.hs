-- | Character sets.
module Kleene.Sets (
    dotRSet,
    ) where

import Data.RangeSet.List  (RSet)

import qualified Data.RangeSet.List as RSet

dotRSet :: RSet Char
dotRSet = RSet.full RSet.\\ RSet.singleton '\n'
