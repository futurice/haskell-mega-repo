{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Schedule.Command.RemoveEmployeesFromSchedule where

import Control.Lens
import Futurice.Generics
import Futurice.IdMap    (Key)
import Futurice.Lomake
import Futurice.Prelude
import Prelude ()

import Futurice.App.Schedule.Command.Definition
import Futurice.App.Schedule.Types.Phase
import Futurice.App.Schedule.Types.Schedule
import Futurice.App.Schedule.Types.World

import qualified Data.Set as S
import qualified Personio as P

data RemoveEmployeeFromSchedule (phase :: Phase) = RemoveEmployeesFromSchedule
    { _reScheduleName :: !(Key Schedule)
    , _reEmployee    :: !P.EmployeeId
    } deriving (GhcGeneric, SopGeneric, HasDatatypeInfo)
      deriving (ToJSON) via (Sopica (RemoveEmployeeFromSchedule 'Done))
      deriving (FromJSON) via (Sopica (RemoveEmployeeFromSchedule 'Input))

instance phase ~ 'Input => HasLomake (RemoveEmployeeFromSchedule phase) where
    lomake _ =
        hiddenField "schedule-name" :*
        dynEnumField "" :*
        Nil

instance Command RemoveEmployeeFromSchedule where
    type CommandTag RemoveEmployeeFromSchedule = "remove-employee-from-schedule"

    processCommand _time _log (RemoveEmployeesFromSchedule sid employee) = pure $ RemoveEmployeesFromSchedule sid employee

    applyCommand _time _login (RemoveEmployeesFromSchedule sid employee) = do
        worldSchedules . ix sid . scheduleEvents %= fmap (\e -> e & eventEmployees .~ S.delete employee (e ^. eventEmployees))
        pure $ CommandResponseReload
