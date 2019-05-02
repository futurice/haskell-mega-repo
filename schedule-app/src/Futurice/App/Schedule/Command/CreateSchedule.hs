{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Schedule.Command.CreateSchedule where

import Control.Lens                      (use, (<>=))
import Data.Time.Format
import Data.Time.LocalTime               (timeOfDayToTime)
import Futurice.Email
import Futurice.Generics
import Futurice.IdMap                    (Key, fromFoldable)
import Futurice.Lomake
import Futurice.Prelude
import Network.Google.AppsCalendar.Types (eId)
import Prelude ()
import Servant.Multipart
       (FromMultipart, Mem, fromMultipart, iName, iValue, inputs, lookupInput)

import Futurice.App.Schedule.Command.Definition
import Futurice.App.Schedule.Types.Phase
import Futurice.App.Schedule.Types.Schedule
import Futurice.App.Schedule.Types.Templates
import Futurice.App.Schedule.Types.World
import Google

import qualified Data.Set  as S
import qualified Data.Text as T
import qualified Personio  as P

data CreateScheduleStart = CreateScheduleStart
    { _csTemplateName :: !(Key ScheduleTemplate)
    , _csStartDate    :: !Day
    , _csEmployees    :: ![P.EmployeeId]
    } deriving Show

instance FromMultipart Mem CreateScheduleStart where
    fromMultipart multipartData = CreateScheduleStart
        <$> lookupInputData "template"
        <*> (lookupInputData "start-date" >>= parseStartDate)
        <*> pure fetchEmployees
      where
        lookupInputData = flip lookupInput multipartData
        parseStartDate = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" . T.unpack
        fetchEmployees = catMaybes $ fmap (fmap P.EmployeeId . readMaybe . T.unpack . iValue) $ filter (\i -> iName i == "employees") $ inputs multipartData

data CreateSchedule (phase :: Phase) = CreateSchedule
    { _csScheduleTemplateId :: !(Key ScheduleTemplate)
    , _csEventSchedule      :: !ScheduleRequest
    , _csCreatedEvents      :: !(Phased phase () () [Maybe Text])
    } deriving (GhcGeneric, SopGeneric, HasDatatypeInfo)

deriveVia [t| forall phase. (phase ~ 'Done => ToJSON (CreateSchedule phase))   `Via` Sopica (CreateSchedule phase) |]
deriveVia [t| FromJSON (CreateSchedule 'Done) `Via` Sopica (CreateSchedule 'Done) |]
deriveVia [t| FromJSON (CreateSchedule 'Input) `Via` Sopica (CreateSchedule 'Input) |]

instance FromMultipart Mem (CreateSchedule 'Input) where
    fromMultipart multipartData =
        let getEventId name = last $ T.splitOn "-" name
            eventIds = S.fromList $ getEventId . iName <$> (filter (\x -> iName x /= "schedule-template") $ inputs multipartData)
            lookupInputWithId iid name = lookupInput (name <> "-" <> iid) multipartData
            buildEvent eventId = EventRequest
                <$> lookupInputWithId eventId "summary"
                <*> lookupInputWithId eventId "description"
                <*> (Just $ fetchLocations eventId)
                <*> (lookupInputWithId eventId "start-date" >>= readMaybe . T.unpack)
                <*> (lookupInputWithId eventId "from" >>= readMaybe . T.unpack)
                <*> (lookupInputWithId eventId "to" >>= readMaybe . T.unpack)
                <*> (Just $ fetchEmployees eventId)
                <*> Just True --TODO: change this to right one
            schedule = sequence (fmap buildEvent $ S.toList eventIds)
            fetchEmployees iid = catMaybes
                $ fmap (fmap P.EmployeeId . readMaybe . T.unpack . iValue)
                $ filter (\i -> iName i == ("employees-" <> iid)) $ inputs multipartData
            fetchLocations iid = fmap iValue $ filter (\i -> iName i == ("locations-" <> iid)) $ inputs multipartData
        in CreateSchedule <$> lookupInput "schedule-template" multipartData <*> schedule <*> pure ()

data CheckedEventRequest = CheckedEventRequest
    { evSummary      :: !Text
    , evDescription  :: !Text
    , evLocations    :: ![Text]   -- TODO: change to location type
    , evStartDate    :: !Day
    , evStartTime    :: !TimeOfDay
    , evEndTime      :: !TimeOfDay
    , evEmployees    :: ![Text]
    , evIsCollective :: !Bool
    }

instance Command CreateSchedule where
    type CommandTag CreateSchedule = "create-schedule"

    processCommand _time _log (CreateSchedule sid schedule _) = do
        employees <- P.personioEmployees
        let emap = fromFoldable employees
        let toChecked ev = do
                emails <- for (ev ^. eventRequestEmployees) $ \emp -> do
                    case emailToText <$> (emap ^? ix emp . P.employeeEmail . _Just) of
                      Just t -> pure t
                      Nothing -> throwError $ "No email for employee " <> show emp <> " found in Personio"
                pure $ CheckedEventRequest
                    { evSummary      = ev ^. eventRequestSummary
                    , evDescription  = ev ^. eventRequestDescription
                    , evLocations    = ev ^. eventRequestLocations
                    , evStartDate    = ev ^. eventStartDate
                    , evStartTime    = ev ^. eventRequestStartTime
                    , evEndTime      = ev ^. eventRequestEndTime
                    , evEmployees    = emails
                    , evIsCollective = ev ^. eventRequestIsCollective
                    }
        let toEvents event
                | evIsCollective event == True = pure $ CalendarEvent
                    { _ceStartTime   = UTCTime (evStartDate event) (timeOfDayToTime $ evStartTime event)
                    , _ceEndTime     = UTCTime (evStartDate event) (timeOfDayToTime $ evEndTime event)
                    , _ceDescription = evDescription event
                    , _ceSummary     = evSummary event
                    , _ceAttendees   = evEmployees event <> evLocations event
                    }
                | otherwise = flip fmap (evEmployees event) $ \emp -> CalendarEvent
                    { _ceStartTime   = UTCTime (evStartDate event) (timeOfDayToTime $ evStartTime event)
                    , _ceEndTime     = UTCTime (evStartDate event) (timeOfDayToTime $ evEndTime event)
                    , _ceDescription = evDescription event
                    , _ceSummary     = evSummary event
                    , _ceAttendees   = [emp] <> evLocations event
                    }
        checkedEvents <- traverse toChecked schedule
        evs <- for (concat $ toEvents <$> checkedEvents) $ \ev ->
          googleSendInvite ev
        pure $ CreateSchedule sid schedule ((^. eId) <$> evs)

    applyCommand time login (CreateSchedule sid schedule evs) = do
        templateName <- use (worldScheduleTemplates . ix sid . scheduleName)
        let eventRequestToEvent er = Event
                { _eventSummary = er ^. eventRequestSummary
                , _eventDescription = er ^. eventRequestDescription
                , _eventLocations = er ^. eventRequestLocations
                , _eventStartTime = er ^. eventRequestStartTime
                , _eventEndTime = er ^. eventRequestEndTime
                , _eventInviteEmployees = False
                , _eventInviteSupervisors = False
                , _eventIsCollective = False
                , _eventEmployees = S.fromList $ er ^. eventRequestEmployees
                }
        worldSchedules <>= (fromFoldable $ [Schedule templateName (fmap eventRequestToEvent schedule) login time evs])
        pure $ CommandResponseOk ()
