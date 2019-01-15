{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DerivingVia       #-}
module Futurice.App.ProxyMgmt.Commands.RemoveEndpoint where

import FUM.Types.Login
import Futurice.Generics
import Futurice.Lomake
import Futurice.Postgres
import Futurice.Prelude
import Prelude ()

import Futurice.App.Proxy.API
import Futurice.App.ProxyMgmt.Ctx
import Futurice.App.ProxyMgmt.Types

data RemoveEndpoint = RemoveEndpoint
    { rePolicy   :: !PolicyName
    , reEndpoint :: !LenientEndpoint
    }
  deriving (Show, Typeable, GhcGeneric)
  deriving anyclass (SopGeneric, HasDatatypeInfo)
  deriving (ToJSON, FromJSON) via (Sopica RemoveEndpoint)

instance HasLomake RemoveEndpoint where
    lomake _ =
        hiddenField "Policy" :*
        hiddenField "Endpoint" :*
        Nil

removeEndpointHandler :: LomakeRequest RemoveEndpoint -> ReaderT (Login, Ctx f) IO (CommandResponse ())
removeEndpointHandler (LomakeRequest e) = ReaderT $ \(login, Ctx {..}) ->
    runLogT "add-endpoint" ctxLogger $ do
        logInfoI "removing endpoint $endpoint from $policy" e

        -- audit
        now <- currentTime
        void $ safePoolExecute ctxPostgresPool
            "INSERT INTO proxyapp.auditlog (username, created, message) VALUES (?, ?, ?);"
            (login, now, textShow e)

        -- execute action
        void $ safePoolExecute ctxPostgresPool
            "DELETE FROM proxyapp.policy_endpoint WHERE policyName = ? AND endpoint = ?"
            (rePolicy e, reEndpoint e)

        -- ok: reload
        return CommandResponseReload
