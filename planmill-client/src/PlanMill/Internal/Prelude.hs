{-# LANGUAGE ExplicitNamespaces #-}
module PlanMill.Internal.Prelude (
    module Futurice.Prelude,
    module Futurice.Time,
    -- * aeson
    FromJSON(..), ToJSON(..),
    withObject, withText,
    object,
    (.:), (.:?), (.=), (.!=),
    Value(..),
    typeMismatch,
    -- ** extras
    getParsedAsText, getParsedAsIntegral,
    -- * binary-tagged
    Structured(..), nominalStructure,
    -- * intervals
    Interval, (...), inf, sup,
    -- * Time related
    ZonedTime, UTCTime(..),
    dayFromZ, zonedTimeDay,
    utcTimeToInteger,
    -- * HasKey
    HasKey (..),
    -- * Type
    type (:~:) (..),
    -- * Misc
    bsShow,
    ) where

import Data.Aeson.Compat
       (FromJSON (..), ToJSON (..), Value (..), object, withObject, withText,
       (.!=), (.:), (.:?), (.=))
import Data.Aeson.Types          (typeMismatch)
import Data.Binary.Tagged        (Structured (..), nominalStructure)
import Data.Time                 (ZonedTime, zonedTimeToLocalTime)
import Data.Time.Clock.POSIX     (utcTimeToPOSIXSeconds)
import Data.Type.Equality
import Futurice.Aeson            (getParsedAsIntegral, getParsedAsText)
import Futurice.IdMap            (HasKey (..))
import Futurice.Prelude
import Futurice.Time
import Numeric.Interval.NonEmpty (Interval, inf, sup, (...))
import Prelude ()

--TODO: remove this function
dayFromZ :: ZonedTime -> Day
dayFromZ = zonedTimeDay

zonedTimeDay :: ZonedTime -> Day
zonedTimeDay = localDay . zonedTimeToLocalTime

bsShow :: Show a => a -> ByteString
bsShow = encodeUtf8 . textShow

utcTimeToInteger :: UTCTime -> Integer
utcTimeToInteger = round . utcTimeToPOSIXSeconds
