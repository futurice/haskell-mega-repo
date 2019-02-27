{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Schedule.Types.World where

import Data.Swagger      (NamedSchema (..), ToSchema (..))
import Futurice.Generics
import Futurice.IdMap    (IdMap)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Schedule.Types.Schedule
import Futurice.App.Schedule.Types.Templates

import qualified Personio as P

data Starter = Starter
    { _starterName       :: !Text
    , _starterEmail      :: !Text
    , _starterSupervisor :: !(Maybe P.Employee)
    , _starterPersonioID :: !(Maybe P.EmployeeId)
    } deriving (Show)

data MeetingRoom = MeetingRoom

data EventTask = EventTask

newtype Identifier a = Identifier UUID
    deriving (Show, Eq, Ord)

data World = World
    { _worldStarters           :: ![Starter]
    , _worldScheduleTemplates  :: !(IdMap ScheduleTemplate)
    , _worldSchedules          :: !(IdMap Schedule)
    } deriving (GhcGeneric, SopGeneric, HasDatatypeInfo)

makeLenses ''World

instance ToSchema World where
    declareNamedSchema _ = pure $ NamedSchema (Just "World") mempty

emptyWorld :: World
emptyWorld = World [] mempty mempty

 -- id            | integer                  |           | not null | nextval('futuschedule_futuuser_id_seq'::regclass)
 -- password      | character varying(128)   |           | not null |
 -- last_login    | timestamp with time zone |           |          |
 -- username      | character varying(40)    |           | not null |
 -- email         | character varying(100)   |           | not null |
 -- first_name    | character varying(100)   |           | not null |
 -- last_name     | character varying(100)   |           | not null |
 -- is_active     | boolean                  |           | not null |
 -- is_admin      | boolean                  |           | not null |
 -- supervisor_id | integer                  |           |          |
 -- name          | character varying(255)   |           |          |
 -- personio_id   | integer                  |           |          |
