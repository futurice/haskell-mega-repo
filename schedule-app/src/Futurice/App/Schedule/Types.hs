{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Futurice.App.Schedule.Types where

import Data.Time.LocalTime
import Futurice.EnvConfig
import Futurice.Generics
import Futurice.IdMap      (HasKey, Key, key)
import Futurice.Prelude
import Prelude ()
import Text.Parsec         (alphaNum, anyChar, many1, parse, sepBy1)
import Text.Parsec.Char    (char)

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

data ScheduleTemplate = ScheduleTemplate
    { _scheduleName       :: !Text
    , _scheduleCalendar   :: !Calendar
    , _scheduleTimezone   :: !TimeZone
    } deriving Show

makeLenses ''ScheduleTemplate

instance HasKey ScheduleTemplate where
    type Key ScheduleTemplate = Text
    key = scheduleName

data SchedulingRequestStatus = Accepted
                             | Declined -- TODO: check what are the actual statuses
                             deriving Show

data SchedulingRequest = SchedulingRequest
    { _srId          :: !Int -- TODO: change this to proper id type like UUID
    , _srJson        :: !Text -- TODO: separate to it's own type
    , _srRequestedAt :: !UTCTime -- should this be localtime?
    , _srStatus      :: !SchedulingRequestStatus
    , _srError       :: !Text -- this should not probably be it's own field?
    , _srRequestedBy :: !P.EmployeeId
    , _srPdfUrl      :: !Text  -- what was this
    } deriving Show

makeLenses ''SchedulingRequest

instance HasKey SchedulingRequest where
    type Key SchedulingRequest = Int
    key = srId
