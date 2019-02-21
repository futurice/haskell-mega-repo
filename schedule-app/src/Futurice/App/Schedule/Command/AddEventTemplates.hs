{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
module Futurice.App.Schedule.Command.AddEventTemplates where

import Futurice.Generics
import Futurice.IdMap    (Key, fromFoldable)
import Futurice.Prelude
import Prelude ()
import Control.Lens      ((<>=))
import Futurice.Lomake

import Futurice.App.Schedule.Command.Definition
import Futurice.App.Schedule.Types.World
import Futurice.App.Schedule.Types.Templates
import Futurice.App.Schedule.Types.Phase

data AddEventTemplates (phase :: Phase) = AddEventTemplates
    { _aeScheduleTemplateName :: !(Key ScheduleTemplate)
    , _aeEventTemplates       :: ![EventTemplate]
    } deriving (GhcGeneric, SopGeneric, HasDatatypeInfo)
      deriving (ToJSON) via (Sopica (AddEventTemplates 'Done))
      deriving (FromJSON) via (Sopica (AddEventTemplates 'Input))

instance ToSchema (AddEventTemplates 'Input) where
    declareNamedSchema = sopDeclareNamedSchema

-- TODO: Add validations

instance Command AddEventTemplates where
    type CommandTag AddEventTemplates = "add-event-templates"

    processCommand _time _log (AddEventTemplates templateId templates) = do
        pure $ AddEventTemplates templateId templates

    applyCommand _time _log (AddEventTemplates templateId templates) = do
        worldScheduleTemplates . ix templateId . scheduleEventTemplates <>= fromFoldable templates
        pure $ CommandResponseOk ()
