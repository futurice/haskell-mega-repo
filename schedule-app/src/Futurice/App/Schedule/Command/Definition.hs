{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TypeFamilies     #-}
module Futurice.App.Schedule.Command.Definition where

import FUM.Types.Login   (Login)
import Futurice.Generics
import Futurice.Lomake   (CommandResponse)
import Futurice.Prelude
import Futurice.Stricter (StricterT)
import Prelude ()

import Futurice.App.Schedule.World

data Phase = Input     -- ^User input
           | Executing -- ^Command that needs executing (for example sending emails)
           | Done      -- ^Command is done

type family Phased (phase :: Phase) a b c where
    Phased 'Input a b c     = a
    Phased 'Executing a b c = b
    Phased 'Done a b c      = c

class (FromJSON (cmd 'Done), FromJSON (cmd 'Input), ToJSON (cmd 'Done), KnownSymbol (CommandTag cmd)) =>  Command (cmd :: Phase -> *) where
    type CommandTag cmd :: Symbol

    processCommand :: UTCTime -> Login -> cmd 'Input -> ReaderT World (ExceptT String (LogT IO)) (cmd 'Done) -- TODO: need for right management?

    applyCommand :: UTCTime -> Login -> cmd 'Done -> StricterT World (Either String) (CommandResponse ()) -- TODO: why not Text?
