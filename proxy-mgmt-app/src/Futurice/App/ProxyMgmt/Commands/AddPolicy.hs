{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DerivingVia       #-}
module Futurice.App.ProxyMgmt.Commands.AddPolicy where

import FUM.Types.Login
import Futurice.Generics
import Futurice.Lomake
import Futurice.Postgres
import Futurice.Prelude
import Prelude ()

import Futurice.App.Proxy.API
import Futurice.App.ProxyMgmt.Ctx
import Futurice.App.ProxyMgmt.Types

data AddPolicy = AddPolicy 
    { addPolicyPolicy :: !PolicyName
    , addPolicyEndpoint :: !Endpoint
    }
    deriving (Show, Typeable, GhcGeneric)
    deriving anyclass (SopGeneric, HasDatatypeInfo)
    deriving (ToJSON, FromJSON) via (Sopica AddPolicy)


instance HasLomake AddPolicy where
    lomake _ =
        textField "Policy name" :*
        enumField "Endpoint" textualToText :*
        Nil


addPolicyHandler :: LomakeRequest AddPolicy -> ReaderT (Login, Ctx) IO (CommandResponse ())
addPolicyHandler (LomakeRequest e) = ReaderT $ \(login, Ctx {..}) ->
    runLogT "add-endpoint" ctxLogger $ do
        logInfoI "creating $policy with $endpoint" e

        -- audit
        now <- currentTime
        void $ safePoolExecute ctxPostgresPool
            "INSERT INTO proxyapp.auditlog (username, created, message) VALUES (?, ?, ?);"
            (login, now, textShow e)

        -- Insert new policy
        void $ safePoolExecute ctxPostgresPool
            "INSERT INTO proxyapp.policy (policyname, created) VALUES (?, ?);"
            (addPolicyPolicy e, now)

        -- execute action
        void $ safePoolExecute ctxPostgresPool
            "INSERT INTO proxyapp.policy_endpoint (policyname, endpoint) VALUES (?, ?);"
            (addPolicyPolicy e, addPolicyEndpoint e)

        -- ok: reload
        return CommandResponseReload
        