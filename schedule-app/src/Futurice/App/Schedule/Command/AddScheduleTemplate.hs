{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
module Futurice.App.Schedule.Command.AddScheduleTemplate where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.Schedule.Command.Definition

data AddScheduleTemplate (phase :: Phase) = AddScheduleTemplate
    { astName :: !Text -- TODO: change to better type
    , astTemp :: !Text -- TODO: remove this
    } deriving Generic

deriveGeneric ''AddScheduleTemplate

deriveVia [t| forall phase. (phase ~ 'Done => ToJSON (AddScheduleTemplate phase))   `Via` Sopica (AddScheduleTemplate phase) |]
deriveVia [t| forall phase. (phase ~ 'Done => FromJSON (AddScheduleTemplate phase)) `Via` Sopica (AddScheduleTemplate phase) |]

instance Command AddScheduleTemplate where
    type CommandTag AddScheduleTemplate = "add-schedule-template"

    processCommand = undefined

    applyCommand = undefined
