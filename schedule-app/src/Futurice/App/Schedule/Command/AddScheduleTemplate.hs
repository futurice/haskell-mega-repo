{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
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
import Futurice.App.Schedule.Types.TimeZoneInfo
import Futurice.App.Schedule.World

import qualified Data.Text as T

data AddScheduleTemplate (phase :: Phase) = AddScheduleTemplate
    { astName     :: !Text -- TODO: change to better type
    , astCalendar :: !Calendar
    , astTimeZone :: !Text
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
        <*> lookupInputData "timezone"
      where
        lookupInputData = flip lookupInput multipartData

instance Command AddScheduleTemplate where
    type CommandTag AddScheduleTemplate = "add-schedule-template"

    processCommand _time _log (AddScheduleTemplate name cal tz) = do
        pure $ AddScheduleTemplate name cal tz

    applyCommand _time _log (AddScheduleTemplate name cal tz) = do
        case textToTimeZone tz of
          Just t -> do
              worldScheduleTemplates . at name ?= ScheduleTemplate
                  { _scheduleName = name
                  , _scheduleCalendar = cal
                  , _scheduleTimezone = t
                  , _scheduleEventTemplates = []
                  }
              pure $ CommandResponseOk ()
          Nothing ->  throwError $ "Given timezone " ++ T.unpack tz ++ " not recognized timezone"

-- validation:
-- unique name
-- existing calendar
