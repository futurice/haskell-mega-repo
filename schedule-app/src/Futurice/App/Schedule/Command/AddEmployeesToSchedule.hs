{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Futurice.App.Schedule.Command.AddEmployeesToSchedule where

import Futurice.IdMap   (Key)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()
import Futurice.Lomake
import Control.Lens

import Futurice.App.Schedule.Command.Definition
import Futurice.App.Schedule.Types.Phase
import Futurice.App.Schedule.Types.Schedule
import Futurice.App.Schedule.Types.World

import qualified Personio as P
import qualified Data.Set as S

data AddEmployeesToSchedule (phase :: Phase) = AddEmployeesToSchedule
    { _aetScheduleName :: !(Key Schedule)
    , _aetEmployees    :: !(P.EmployeeId)
    } deriving (GhcGeneric, SopGeneric, HasDatatypeInfo)
      deriving (ToJSON) via (Sopica (AddEmployeesToSchedule 'Done))
      deriving (FromJSON) via (Sopica (AddEmployeesToSchedule 'Input))

instance phase ~ 'Input => HasLomake (AddEmployeesToSchedule phase) where
    lomake _ =
        hiddenField "schedule-name" :*
        dynEnumField "" :*
        Nil

instance Command AddEmployeesToSchedule where
    type CommandTag AddEmployeesToSchedule = "add-employees-to-schedule"

    processCommand _time _log (AddEmployeesToSchedule sid employees) = pure $ AddEmployeesToSchedule sid employees

    applyCommand _time _login (AddEmployeesToSchedule sid employees) = do
        worldSchedules . ix sid . scheduleEvents %= fmap (\e -> e & eventEmployees <>~ S.fromList [employees])
        pure $ CommandResponseReload
