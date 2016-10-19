-- | /TODO:/ remove
module Data.Time.Fxtra (
    beginningOfPrevMonth,
    module Data.Time,
    ) where

import Futurice.Prelude

import Data.Time

beginningOfPrevMonth :: Day -> Day
beginningOfPrevMonth = fromGregorian' . f. toGregorian
  where
    f (y, 1, _) = (y - 1, 12, 1)
    f (y, m, _) = (y, m - 1, 1)

fromGregorian' :: (Integer, Int, Int) -> Day
fromGregorian' (y, m, d) = fromGregorian y m d
