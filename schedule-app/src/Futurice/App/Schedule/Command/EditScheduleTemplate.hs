{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Schedule.Command.EditScheduleTemplate where

import Control.Lens      ((.=))
import Futurice.Generics
import Futurice.IdMap    (Key)
import Futurice.Lomake
import Futurice.Prelude
import Prelude ()

import Futurice.App.Schedule.Command.Definition
import Futurice.App.Schedule.Types.Identifier
import Futurice.App.Schedule.Types.Phase
import Futurice.App.Schedule.Types.Templates
import Futurice.App.Schedule.Types.World

import qualified Data.Text      as T
import qualified Kleene.Functor as K

data EditEventTemplate (phase :: Phase) = EditEventTemplate
    { _estScheduleTemplateName :: !(Key ScheduleTemplate)
    , _estEventTemplateId      :: !(Identifier EventTemplate)
    , _estSummary              :: !Text
    , _estDescription          :: !Text
    , _estTimeOffset           :: !(Either DayOffset MonthOffset)
    , _estStartTime            :: !TimeOfDay
    , _estEndTime              :: !TimeOfDay
    , _estInviteEmployees      :: !Bool
    , _estInviteSupervisors    :: !Bool
    , _estIsCollective         :: !Bool
    } deriving (GhcGeneric, SopGeneric, HasDatatypeInfo)

deriveVia [t| forall phase. (phase ~ 'Done => ToJSON (EditEventTemplate phase)) `Via` Sopica (EditEventTemplate phase) |]
deriveVia [t| FromJSON (EditEventTemplate 'Done) `Via` Sopica (EditEventTemplate 'Done) |]
deriveVia [t| FromJSON (EditEventTemplate 'Input) `Via` Sopica (EditEventTemplate 'Input) |]

instance ToSchema (EditEventTemplate 'Input) where
    declareNamedSchema = sopDeclareNamedSchema

instance ToHtml (Either DayOffset MonthOffset) where
    toHtmlRaw = toHtml
    toHtml = undefined

instance ToHtml Bool where
    toHtmlRaw = toHtml
    toHtml = undefined

boolToText :: Bool -> Text
boolToText = T.pack . show

instance phase ~ 'Input => HasLomake (EditEventTemplate phase) where
    lomake _ =
        hiddenField "schedule-template-name" :*
        hiddenField "event-template-id" :*
        textField "summary" :*
        textField "description" :*
        textField "offset" :*
        textFieldWithRegexp "start-time" timeKleene :*
        textFieldWithRegexp "end-time" timeKleene :*
        boolField "invite-employees" :*
        boolField "invite-supervisors" :*
        textField "is-collective" :*
        Nil

timeKleene :: K.K Char TimeOfDay
timeKleene = read
             <$> (pure <$> ((K.charRange '0' '2'
             *> K.charRange '0' '9'
             *> K.char ':'
             *> K.charRange '0' '5'
             *> K.charRange '0' '9') <|> (K.charRange '0' '9'
             *> K.char ':'
             *> K.charRange '0' '5'
             *> K.charRange '0' '9')))

-- TODO: Add validations

instance Command EditEventTemplate where
    type CommandTag EditEventTemplate = "edit-schedule-template"

    processCommand _time _log e = do
        pure $ EditEventTemplate
            { _estScheduleTemplateName = _estScheduleTemplateName e
            , _estEventTemplateId      = _estEventTemplateId e
            , _estSummary              = _estSummary e
            , _estDescription          = _estDescription e
            , _estTimeOffset           = _estTimeOffset e
            , _estStartTime            = _estStartTime e
            , _estEndTime              = _estEndTime e
            , _estInviteEmployees      = _estInviteEmployees e
            , _estInviteSupervisors    = _estInviteSupervisors e
            , _estIsCollective         = _estIsCollective e
            }

    applyCommand _time _log e = do
        let eventTemplate = EventTemplate
                { _etEventTemplateId   = _estEventTemplateId e
                , _etSummary           = _estSummary e
                , _etDescription       = _estDescription e
                , _etTimeOffset        = _estTimeOffset e
                , _etStartTime         = _estStartTime e
                , _etEndTime           = _estEndTime e
                , _etInviteEmployees   = _estInviteEmployees e
                , _etInviteSupervisors = _estInviteSupervisors e
                , _etIsCollective      = _estIsCollective e
                }
        worldScheduleTemplates . ix (_estScheduleTemplateName e) . scheduleEventTemplates . ix (_estEventTemplateId e) .= eventTemplate
        pure $ CommandResponseOk ()
