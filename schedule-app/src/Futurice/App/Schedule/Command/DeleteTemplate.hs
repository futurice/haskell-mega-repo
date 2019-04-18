{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Futurice.App.Schedule.Command.DeleteTemplate where

import Control.Lens
import Control.Monad.State.Class
import Futurice.Generics
import Futurice.IdMap            (Key)
import Futurice.Lomake
import Futurice.Prelude          hiding (Binary (..))
import Prelude ()

import Futurice.App.Schedule.Command.Definition
import Futurice.App.Schedule.Types.Phase
import Futurice.App.Schedule.Types.Templates
import Futurice.App.Schedule.Types.World

newtype DeleteTemplate (phase :: Phase) = DeleteTemplate (Key ScheduleTemplate)
    deriving (GhcGeneric)
    deriving anyclass (ToJSON, FromJSON, ToSchema, SopGeneric, HasDatatypeInfo)

instance Command DeleteTemplate where
    type CommandTag DeleteTemplate = "delete-template"

    processCommand _time _log (DeleteTemplate t) = pure $ DeleteTemplate t

    applyCommand _time _log (DeleteTemplate t) = do
        world <- ask
        let w = world & worldScheduleTemplates . at t .~ Nothing
        put w
        pure $ CommandResponseOk ()

-- TODO: Add validations
