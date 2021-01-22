{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Futurice.App.Contacts (defaultMain) where

import Control.Concurrent.STM
       (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Futurice.Integrations
import Futurice.Periocron
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant

-- Contacts modules
import Futurice.App.Contacts.API
import Futurice.App.Contacts.Config
import Futurice.App.Contacts.Logic
import Futurice.App.Contacts.Types

type Ctx = TVar [Contact Text]

server :: Ctx -> Server ContactsAPI
server action = liftIO (readTVarIO action)
    :<|> liftIO (readTVarIO action)
    :<|> liftIO (readTVarIO action)

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService         .~ ContactsApiService
    & serverDescription     .~ "All employees and externals"
    & serverApp contactsApi .~ server
    & serverColour          .~  (Proxy :: Proxy ('FutuAccent 'AF4 'AC1))
    & serverEnvPfx          .~ "CONTACTSAPI"
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
    makeCtx cfg lgr mgr _cache mq = do
        cs <- newTVarIO []
        -- Action returning the contact list
        let action = do
                runLogT "update-contacts" lgr $ logTrace_ "Updating"
                now <- currentTime
                c <- runIntegrations mgr lgr now cfg contacts
                atomically $ writeTVar cs c

        -- Periodically try to fetch new data
        let job = mkJob "update contacts" action $ every 3600

        -- listen to MQ, especially updated Personio
        void $ forEachMessage mq $ \msg -> case msg of
          PersonioUpdated -> action
          _               -> pure ()

        pure (cs, [job])
