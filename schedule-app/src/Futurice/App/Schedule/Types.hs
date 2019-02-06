{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Futurice.App.Schedule.Types where

import Data.Time.LocalTime
import Futurice.IdMap      (HasKey, IdMap, Key, key)
import Futurice.Prelude
import Prelude ()

data Calendar = Calendar deriving Show

data ScheduleTemplate = ScheduleTemplate
    { _scheduleName       :: !Text
    , _scheduleCalendar   :: !Calendar
    , _scheduleTimezone   :: !TimeZone
    } deriving Show

makeLenses ''ScheduleTemplate

instance HasKey ScheduleTemplate where
    type Key ScheduleTemplate = Text
    key = scheduleName
