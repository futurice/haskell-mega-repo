{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.Schedule.Command.DeleteSchedule where

import Futurice.Generics
import Futurice.Lomake
import Futurice.Prelude
import Google
import Prelude ()

import Futurice.App.Schedule.Command.Definition
import Futurice.App.Schedule.Types.Phase
import Futurice.App.Schedule.Types.Schedule
import Futurice.App.Schedule.Types.World

import qualified Futurice.IdMap as IdMap

newtype DeleteSchedule (phase :: Phase) = DeleteSchedule {scheduleName :: (IdMap.Key Schedule)}
    deriving (GhcGeneric)
    deriving anyclass (FromJSON, ToJSON, HasDatatypeInfo, SopGeneric)

instance phase ~ 'Input => HasLomake (DeleteSchedule phase) where
    lomake _ =
        hiddenField "schedule-name" :*
        Nil

instance Command DeleteSchedule where
    type CommandTag DeleteSchedule = "delete-schedule"

    processCommand _time _log (DeleteSchedule scheduleId) = do
        world <- commandWorld <$> ask
        for_ (world ^. worldSchedules . at scheduleId) $ \s -> do
            for_ (s ^. scheduleEventIds) $ \eid -> do
                for_ eid $ \e -> do
                    googleDeleteEvent e
        pure $ DeleteSchedule scheduleId

    applyCommand _time _login (DeleteSchedule scheduleId) = do
        worldSchedules %= IdMap.filter (\s -> s ^. IdMap.key /= scheduleId)
        pure $ CommandResponseOk ()
