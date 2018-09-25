{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.ResultInterval (
    ResultInterval(..),
    IntervalType(..),
    intervalToQueryString,
    -- * Interval
    Interval,
    (...),
    elimInterval,
    ) where

import PlanMill.Internal.Prelude
import PlanMill.Types.Request    (QueryString)
import PlanMill.Types.UOffset    (showPlanmillDay)

import qualified Data.Map as Map

-- | Interval field.
data IntervalType
    = IntervalStart
    | IntervalFinish
    | IntervalCreated
    | IntervalModified
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Generic, Typeable)

-- | Map to query string value.
intervalToQueryString :: ResultInterval -> QueryString
intervalToQueryString (ResultInterval t i) =
    -- This is tricky, not well documented.
    -- We have inclusive day interval, and need to translate it into
    -- something API treats well
    Map.fromList [ ("interval", t')
    , ("intervalstart", fromString . showPlanmillDay $ inf i)
    , ("intervalfinish", fromString . showPlanmillDay $ succ $ sup i)
    ]
  where
    t' = case t of
        IntervalStart    -> "start"
        IntervalFinish   -> "finish"
        IntervalCreated  -> "created"
        IntervalModified -> "modified"

-- | Result interval.
data ResultInterval = ResultInterval IntervalType (Interval Day)
    deriving (Eq, Ord, Show, Generic, Typeable)

elimInterval :: (a -> a -> b) -> Interval a -> b
elimInterval f i = f (inf i) (sup i)
