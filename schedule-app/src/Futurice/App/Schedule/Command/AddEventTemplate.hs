{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Schedule.Command.AddEventTemplate where

import Control.Lens      ((<>=))
import Futurice.Generics
import Futurice.IdMap    (Key, fromFoldable)
import Futurice.Lomake
import Futurice.Prelude
import Prelude ()

import Futurice.App.Schedule.Command.Definition
import Futurice.App.Schedule.Types.Phase
import Futurice.App.Schedule.Types.Templates
import Futurice.App.Schedule.Types.World

data AddEventTemplate (phase :: Phase) = AddEventTemplate
    { _addeScheduleTemplateName :: !(Key ScheduleTemplate)
    , _addeEventTemplateSummary :: !Text
    , _addeNewEventTemplate     :: !(Phased phase () () EventTemplate)
    } deriving (GhcGeneric, SopGeneric, HasDatatypeInfo)
      -- deriving (ToJSON) via (Sopica (AddEventTemplate 'Done))
      -- deriving (FromJSON) via (Sopica (AddEventTemplate 'Input))

deriveVia [t| forall phase. (phase ~ 'Done => ToJSON (AddEventTemplate phase))   `Via` Sopica (AddEventTemplate phase) |]
deriveVia [t| FromJSON (AddEventTemplate 'Done) `Via` Sopica (AddEventTemplate 'Done) |]
deriveVia [t| FromJSON (AddEventTemplate 'Input) `Via` Sopica (AddEventTemplate 'Input) |]

instance phase ~ 'Input => HasLomake (AddEventTemplate phase) where
    lomake _ =
        hiddenField "schedule-name" :*
        textField "Summary" :*
        unitField :*
        Nil

instance ToSchema (AddEventTemplate 'Input) where
    declareNamedSchema = sopDeclareNamedSchema

-- TODO: Add validations

instance Command AddEventTemplate where
    type CommandTag AddEventTemplate = "add-event-template"

    processCommand _time _log (AddEventTemplate templateId summary _) = do
        eventTemplate <- liftIO $ emptyEventTemplate
        pure $ AddEventTemplate templateId summary eventTemplate

    applyCommand _time _log (AddEventTemplate templateId summary eventTemplate)= do
        worldScheduleTemplates . ix templateId . scheduleEventTemplates <>= fromFoldable [eventTemplate { _etSummary = summary}]
        pure $ CommandResponseReload
