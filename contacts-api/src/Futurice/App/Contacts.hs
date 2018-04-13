{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Contacts (defaultMain) where

import Prelude ()
import Futurice.Prelude
import Futurice.Integrations
import Futurice.Periocron
import Futurice.Servant
import Servant

-- Contacts modules
import Futurice.App.Contacts.API
import Futurice.App.Contacts.Config
import Futurice.App.Contacts.Logic
import Futurice.App.Contacts.Types

type Ctx = IO [Contact Text]

server :: Ctx -> Server ContactsAPI
server action = liftIO action
    :<|> liftIO action
    :<|> liftIO action

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService         .~ ContactsApiService
    & serverDescription     .~ "All employees and externals"
    & serverApp contactsApi .~ server
    & serverColour          .~  (Proxy :: Proxy ('FutuAccent 'AF4 'AC1))
    & serverEnvPfx          .~ "CONTACTSAPI"
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> IO (Ctx, [Job])
    makeCtx cfg lgr mgr cache = do
        now <- currentTime

        -- Contacts action
        let getContacts = runIntegrations mgr lgr now cfg contacts

        -- Action returning the contact list
        let action = cachedIO lgr cache 3600 () getContacts

        -- Periodically try to fetch new data
        let job = mkJob "update contacts" action $ every 300

        pure (action, [job])
