{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Schedule.Command.AddScheduleTemplate where

import Data.Time.LocalTime
import Futurice.Generics
import Futurice.Lomake
import Futurice.Prelude
import Prelude ()

import Futurice.App.Schedule.Command.Definition
import Futurice.App.Schedule.Types
import Futurice.App.Schedule.World

data AddScheduleTemplate (phase :: Phase) = AddScheduleTemplate
    { astName :: !Text -- TODO: change to better type
    , astTemp :: !Text -- TODO: remove this
    } deriving (Show, Generic)

deriveGeneric ''AddScheduleTemplate

deriveVia [t| forall phase. (phase ~ 'Done => ToJSON (AddScheduleTemplate phase))   `Via` Sopica (AddScheduleTemplate phase) |]
deriveVia [t| (FromJSON (AddScheduleTemplate 'Input)) `Via` Sopica (AddScheduleTemplate 'Input) |]
deriveVia [t| (FromJSON (AddScheduleTemplate 'Done)) `Via` Sopica (AddScheduleTemplate 'Done) |]

instance ToSchema (AddScheduleTemplate 'Input) where
    declareNamedSchema = sopDeclareNamedSchema

instance Command AddScheduleTemplate where
    type CommandTag AddScheduleTemplate = "add-schedule-template"

    processCommand time log (AddScheduleTemplate name temp) = do
        pure $ AddScheduleTemplate name temp

    applyCommand time log (AddScheduleTemplate name temp) = do
        worldScheduleTemplates . at name ?= ScheduleTemplate
            { _scheduleName = name
            , _scheduleCalendar = Calendar
            , _scheduleTimezone = utc -- TODO: Change to right one
            }
        pure $ CommandResponseOk ()

-- validation:
-- unique name
