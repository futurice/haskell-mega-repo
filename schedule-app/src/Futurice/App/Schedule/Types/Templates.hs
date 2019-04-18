{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Futurice.App.Schedule.Types.Templates where

import Futurice.Prelude
import Futurice.IdMap
import Prelude ()
import Futurice.Generics
import Futurice.IdMap      (HasKey, Key, key)
import Futurice.EnvConfig
import Text.Parsec         (alphaNum, many1, parse, sepBy1)
import Text.Parsec.Char    (char)
import Data.Swagger        (NamedSchema (..), ToSchema (..))
import Data.UUID.V4 (nextRandom)
import Data.Time.LocalTime (midnight)

import Futurice.App.Schedule.Types.TimeZoneInfo
import Futurice.App.Schedule.Types.Identifier

import qualified Data.Text as T

newtype MonthOffset = MonthOffset Integer
    deriving (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
    deriving newtype (FromHttpApiData, ToHttpApiData)

newtype DayOffset = DayOffset Integer
    deriving (Show, Generic, Ord, Eq)
    deriving anyclass (FromJSON, ToJSON, ToSchema)
    deriving newtype (FromHttpApiData, ToHttpApiData)

getOffset :: DayOffset -> Integer
getOffset (DayOffset i) = i

newtype Calendar = Calendar Text
    deriving (Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

calendarToText :: Calendar -> Text
calendarToText (Calendar cal) = cal

instance FromEnvVarList Calendar where
    fromEnvVarList = either (const Nothing) Just . parse p "<input>"
      where
        p = (fmap . fmap) (Calendar . T.pack) $ many1 (alphaNum <|> char '.' <|> char '@') `sepBy1` comma
        comma = char ','

data EventTemplate = EventTemplate
    { _etEventTemplateId   :: !(Identifier EventTemplate)
    , _etSummary           :: !Text
    , _etDescription       :: !Text
    , _etTimeOffset        :: !(Either DayOffset MonthOffset) -- TODO two different offsets?
    , _etStartTime         :: !TimeOfDay
    , _etEndTime           :: !TimeOfDay
    , _etInviteEmployees   :: !Bool
    , _etInviteSupervisors :: !Bool
    , _etIsCollective      :: !Bool
    } deriving (Show, GhcGeneric, SopGeneric, HasDatatypeInfo)
      deriving (FromJSON, ToJSON) via (Sopica EventTemplate)

instance Entity EventTemplate where entityName _ = "EventTemplate"

instance ToSchema TimeOfDay where
    declareNamedSchema _p = pure $ NamedSchema (Just "TimeOfDay") $ mempty

instance ToSchema EventTemplate where
    declareNamedSchema _p = pure $ NamedSchema (Just "EventTemplate") $ mempty

makeLenses ''EventTemplate

instance HasKey EventTemplate where
    type Key EventTemplate = Identifier EventTemplate
    key = etEventTemplateId

emptyEventTemplate :: IO EventTemplate
emptyEventTemplate = do
    newUUID <- nextRandom
    pure EventTemplate
        { _etEventTemplateId   = Identifier newUUID
        , _etSummary           = ""
        , _etDescription       = ""
        , _etTimeOffset        = Left $ DayOffset 0
        , _etStartTime         = midnight
        , _etEndTime           = midnight
        , _etInviteEmployees   = False
        , _etInviteSupervisors = False
        , _etIsCollective      = False
        }

instance ToJSON (IdMap EventTemplate) where
    toJSON = toJSON . toMap

data ScheduleTemplate = ScheduleTemplate
    { _scheduleName             :: !Text
    , _scheduleCalendar         :: !Calendar
    , _scheduleTimezone         :: !TimeZoneInfo
    , _scheduleEventTemplates   :: !(IdMap EventTemplate)
    } deriving (Show, GhcGeneric, SopGeneric, HasDatatypeInfo)
      deriving (ToJSON) via (Sopica ScheduleTemplate)

makeLenses ''ScheduleTemplate

instance HasKey ScheduleTemplate where
    type Key ScheduleTemplate = Text
    key = scheduleName

instance ToSchema ScheduleTemplate where
    declareNamedSchema _ = pure $ NamedSchema (Just "Schedule template") mempty
