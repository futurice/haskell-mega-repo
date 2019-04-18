{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableSuperClasses #-}
module Futurice.App.Schedule.Command.Definition where

import FUM.Types.Login       (Login)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Lomake       (CommandResponse)
import Futurice.Prelude
import Futurice.Stricter     (StricterT)
import Prelude ()

import Futurice.App.Schedule.Types.Phase
import Futurice.App.Schedule.Types.World

class ( FromJSON (cmd 'Done), FromJSON (cmd 'Input), ToJSON (cmd 'Done), KnownSymbol (CommandTag cmd)) =>  Command (cmd :: Phase -> *) where
    type CommandTag cmd :: Symbol

    processCommand :: UTCTime -> Login -> cmd 'Input -> ReaderT (World, (Manager, Logger, IntegrationsConfig '[ServGO, ServPE])) (ExceptT String (LogT IO)) (cmd 'Done) -- TODO: need for right management?

    applyCommand :: UTCTime -> Login -> cmd 'Done -> StricterT World (Either String) (CommandResponse ()) -- TODO: why not Text?
