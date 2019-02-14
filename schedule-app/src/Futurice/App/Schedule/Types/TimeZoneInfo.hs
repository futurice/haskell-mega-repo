{-# LANGUAGE TemplateHaskell #-}
module Futurice.App.Schedule.Types.TimeZoneInfo where

import Data.Aeson
import Data.Time.Zones
import Data.Time.Zones.All
import Futurice.Prelude
import Prelude ()

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Set              as S
import qualified Data.Text             as T

newtype TimeZoneInfo = TimeZoneInfo Text
    deriving Show

instance ToJSON TimeZoneInfo where
    toJSON (TimeZoneInfo t) = toJSON t

timeZoneToText :: TimeZoneInfo -> Text
timeZoneToText (TimeZoneInfo t) = t

officeTimeLabelsStrings :: [String]
officeTimeLabelsStrings = BS8.unpack . toTZName <$> toList officeTimeLabels

officeTimeLabels :: S.Set TZLabel
officeTimeLabels = let officetimezones = $(makeRelativeToProject "timezones.json" >>= embedFromJSON (Proxy :: Proxy [Text]))
                       officelabels = catMaybes $ fromTZName <$> (BS8.pack <$> (T.unpack <$> officetimezones))
                   in S.fromList officelabels

officeTimeZones :: [TZ]
officeTimeZones = let officetimezones = $(makeRelativeToProject "timezones.json" >>= embedFromJSON (Proxy :: Proxy [Text]))
                      officelabels = catMaybes $ fromTZName <$> (BS8.pack <$> (T.unpack <$> officetimezones))
                      officetzs = tzByLabel <$> officelabels
                  in officetzs

textToTimeZone :: Text -> Maybe TimeZoneInfo
textToTimeZone t = do
    tzName <- fromTZName $ BS8.pack $ T.unpack t
    if S.member tzName officeTimeLabels then Just (TimeZoneInfo t) else Nothing
