{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Schedule.Types where

import Data.Time.LocalTime
import Futurice.EnvConfig
import Futurice.Generics
import Futurice.IdMap      (HasKey, Key, key)
import Data.Swagger     (NamedSchema (..), ToSchema (..))
import Futurice.Prelude
import Prelude ()
import Text.Parsec         (alphaNum, many1, parse, sepBy1)
import Text.Parsec.Char    (char)

import Futurice.App.Schedule.Types.TimeZoneInfo

import qualified Data.Text as T
import qualified Personio  as P

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

newtype MonthOffset = MonthOffset Integer deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

newtype DayOffset = DayOffset Integer deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data EventTemplate = EventTemplate
    { _etSummary           :: !Text
    , _etDescription       :: !Text
    , _etTimeOffset        :: !(Either DayOffset MonthOffset) -- TODO two different offsets?
    , _etStartTime         :: !NominalDiffTime -- TODO: Duration?3
    , _etEndTime           :: !NominalDiffTime
    , _etInviteEmployees   :: !Bool
    , _etInviteSupervisors :: !Bool
    , _etIsCollective      :: !Bool
    } deriving (Show, GhcGeneric, SopGeneric, HasDatatypeInfo)
      deriving (FromJSON, ToJSON) via (Sopica EventTemplate)

instance ToSchema EventTemplate where
    declareNamedSchema = sopDeclareNamedSchema

data ScheduleTemplate = ScheduleTemplate
    { _scheduleName             :: !Text
    , _scheduleCalendar         :: !Calendar
    , _scheduleTimezone         :: !TimeZoneInfo
    , _scheduleEventTemplates   :: ![EventTemplate]
    } deriving (Show, GhcGeneric, SopGeneric, HasDatatypeInfo)
      deriving (ToJSON) via (Sopica ScheduleTemplate)

makeLenses ''ScheduleTemplate

instance HasKey ScheduleTemplate where
    type Key ScheduleTemplate = Text
    key = scheduleName

instance ToSchema ScheduleTemplate where
    declareNamedSchema _ = pure $ NamedSchema (Just "Schedule template") mempty

data SchedulingRequestStatus = Accepted
                             | Declined -- TODO: check what are the actual statuses
                             deriving Show

data SchedulingRequest = SchedulingRequest
    { _srId             :: !Int -- TODO: change this to proper id type like UUID
    , _srJson           :: !Text -- TODO: separate to it's own type
    , _srRequestedAt    :: !UTCTime -- should this be localtime?
    , _srStatus         :: !SchedulingRequestStatus
    , _srError          :: !Text -- this should not probably be it's own field?
    , _srRequestedBy    :: !P.EmployeeId
    , _srPdfUrl         :: !Text  -- what was this
    } deriving Show

makeLenses ''SchedulingRequest

instance HasKey SchedulingRequest where
    type Key SchedulingRequest = Int
    key = srId
