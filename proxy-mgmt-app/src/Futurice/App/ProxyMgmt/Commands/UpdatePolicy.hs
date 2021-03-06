{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DerivingVia       #-}
module Futurice.App.ProxyMgmt.Commands.UpdatePolicy where

import FUM.Types.Login
import Futurice.Generics
import Futurice.Lomake
import Futurice.Postgres
import Futurice.Prelude
import Prelude ()

import Futurice.App.Proxy.API
import Futurice.App.ProxyMgmt.Ctx
import Futurice.App.ProxyMgmt.Types
import Futurice.App.ProxyMgmt.Utils

data UpdatePolicy = UpdatePolicy
    { updatePolicyUser   :: !UserName
    , updatePolicyPolicy :: !PolicyName
    }
  deriving (Show, Typeable, GhcGeneric)
  deriving anyclass (SopGeneric, HasDatatypeInfo)
  deriving (ToJSON, FromJSON) via (Sopica UpdatePolicy)

instance HasLomake UpdatePolicy where 
    lomake _ =
        hiddenField "Username" :*
        dynEnumField "Policy" :*
        Nil

updatePolicyHandler :: LomakeRequest UpdatePolicy -> ReaderT (Login, Ctx) IO (CommandResponse ())
updatePolicyHandler (LomakeRequest e) = ReaderT $ \(login, Ctx {..}) -> do
    runLogT "update-policy" ctxLogger $ do
        logInfoI "updating policy for $user with $policy" e

        -- audit
        now <- currentTime
        void $ safePoolExecute ctxPostgresPool 
            "INSERT INTO proxyapp.auditlog (username, created, message) VALUES (?, ?, ?)"
            (login, now, textShow e)

        -- execute action
        void $ safePoolExecute ctxPostgresPool
            "UPDATE proxyapp.credentials SET policyname = ? where username = ?"
            (updatePolicyPolicy e, updatePolicyUser e)

        -- ok: reload
        return CommandResponseReload