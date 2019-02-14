{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Futurice.App.Schedule.Command.CreateSchedule where

import Data.Time.Format
import Futurice.Prelude
import Prelude ()
import Servant.Multipart
       (FromMultipart, Mem, fromMultipart, iName, iValue, inputs, lookupInput)

import qualified Data.Text as T
import qualified Personio  as P

data CreateScheduleStart = CreateScheduleStart
    { _csTemplateName :: !Text
    , _csStartDate    :: !UTCTime
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
