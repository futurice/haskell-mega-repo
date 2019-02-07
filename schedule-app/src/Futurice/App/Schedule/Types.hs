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
