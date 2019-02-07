{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Schedule.Command.AddScheduleTemplate where

import Control.Lens.Getter (use)
import Data.Time.LocalTime
import Futurice.Generics
import Futurice.Lomake
import Futurice.Prelude
import Prelude ()
import Servant.Multipart   (FromMultipart, Mem, fromMultipart, lookupInput)

import Futurice.App.Schedule.Command.Definition
import Futurice.App.Schedule.Types
import Futurice.App.Schedule.World

data AddScheduleTemplate (phase :: Phase) = AddScheduleTemplate
    { astName     :: !Text -- TODO: change to better type
    , astCalendar :: !Calendar
    } deriving (Show, Generic)

deriveGeneric ''AddScheduleTemplate

deriveVia [t| forall phase. (phase ~ 'Done => ToJSON (AddScheduleTemplate phase))   `Via` Sopica (AddScheduleTemplate phase) |]
deriveVia [t| (FromJSON (AddScheduleTemplate 'Input)) `Via` Sopica (AddScheduleTemplate 'Input) |]
deriveVia [t| (FromJSON (AddScheduleTemplate 'Done)) `Via` Sopica (AddScheduleTemplate 'Done) |]

instance ToSchema (AddScheduleTemplate 'Input) where
    declareNamedSchema = sopDeclareNamedSchema

instance FromMultipart Mem (AddScheduleTemplate 'Input) where
    fromMultipart multipartData = AddScheduleTemplate
        <$> lookupInputData "name"
        <*> (Calendar <$> lookupInputData "calendar")
      where
        lookupInputData = flip lookupInput multipartData

instance Command AddScheduleTemplate where
    type CommandTag AddScheduleTemplate = "add-schedule-template"

    processCommand time log (AddScheduleTemplate name cal) = do
        pure $ AddScheduleTemplate name cal

    applyCommand time log (AddScheduleTemplate name cal) = do
        worldScheduleTemplates . at name ?= ScheduleTemplate
            { _scheduleName = name
            , _scheduleCalendar = cal
            , _scheduleTimezone = utc -- TODO: Change to right one
            }
        pure $ CommandResponseOk ()

-- validation:
-- unique name
-- existing calendar
