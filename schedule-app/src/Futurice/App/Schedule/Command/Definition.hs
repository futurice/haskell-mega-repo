{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
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

data CommandConfig = CommandConfig
    { commandWorld             :: !World
    , commandManager           :: !Manager
    , commandLogger            :: !Logger
    , commandIntegrationConfig :: !(IntegrationsConfig '[ServGO, ServPE])
    }

newtype ProcessMonad a = ProcessMonad { runProcessMonad :: ReaderT CommandConfig (ExceptT String (LogT IO)) a }
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTime, MonadReader CommandConfig, MonadError String)

instance MonadGoogle ProcessMonad where
    googleReq a = do
        cfg <- ask
        now <- currentTime
        liftIO $ runIntegrations (commandManager cfg) (commandLogger cfg) now (commandIntegrationConfig cfg) $ googleReq a

instance MonadPersonio ProcessMonad where
    personio a = do
        cfg <- ask
        now <- currentTime
        liftIO $ runIntegrations (commandManager cfg) (commandLogger cfg) now (commandIntegrationConfig cfg) $ personio a

class ( FromJSON (cmd 'Done), FromJSON (cmd 'Input), ToJSON (cmd 'Done), KnownSymbol (CommandTag cmd)) => Command (cmd :: Phase -> *) where
    type CommandTag cmd :: Symbol

    processCommand :: UTCTime -> Login -> cmd 'Input -> ProcessMonad (cmd 'Done) -- TODO: need for right management?

    applyCommand :: UTCTime -> Login -> cmd 'Done -> StricterT World (Either String) (CommandResponse ()) -- TODO: why not Text?
