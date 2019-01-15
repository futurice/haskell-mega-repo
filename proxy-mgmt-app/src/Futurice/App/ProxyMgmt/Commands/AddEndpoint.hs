{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DerivingVia       #-}
module Futurice.App.ProxyMgmt.Commands.AddEndpoint where

import FUM.Types.Login
import Futurice.Generics
import Futurice.Lomake
import Futurice.Postgres
import Futurice.Prelude
import Prelude ()

import Futurice.App.Proxy.API
import Futurice.App.ProxyMgmt.Ctx
import Futurice.App.ProxyMgmt.Types

data AddEndpoint = AddEndpoint
    { addEndpointPolicy   :: !PolicyName
    , addEndpointEndpoint :: !Endpoint
    }
  deriving (Show, Typeable, GhcGeneric)
  deriving anyclass (SopGeneric, HasDatatypeInfo)
  deriving (ToJSON, FromJSON) via (Sopica AddEndpoint)

instance HasLomake AddEndpoint where
    lomake _ =
        hiddenField "Policy" :*
        enumField "Endpoint" textualToText :*
        Nil

addEndpointHandler :: LomakeRequest AddEndpoint -> ReaderT (Login, Ctx f) IO (CommandResponse ())
addEndpointHandler (LomakeRequest e) = ReaderT $ \(login, Ctx {..}) ->
    runLogT "add-endpoint" ctxLogger $ do
        logInfoI "adding endpoint $endpoint to $policy" e

        -- audit
        now <- currentTime
        void $ safePoolExecute ctxPostgresPool
            "INSERT INTO proxyapp.auditlog (username, created, message) VALUES (?, ?, ?);"
            (login, now, textShow e)

        -- execute action
        void $ safePoolExecute ctxPostgresPool
            "INSERT INTO proxyapp.policy_endpoint (policyname, endpoint) VALUES (?, ?);"
            (addEndpointPolicy e, addEndpointEndpoint e)

        -- ok: reload
        return CommandResponseReload
