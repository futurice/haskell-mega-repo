{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.ProxyMgmt.Commands.AddToken where

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

data AddToken = AddToken
    { addTokenLogin  :: !Login
    , addTokenPolicy :: !PolicyName
    }
  deriving (Show, Typeable, Generic)

deriveGeneric ''AddToken
deriveVia [t| ToJSON AddToken   `Via` Sopica AddToken |]
deriveVia [t| FromJSON AddToken `Via` Sopica AddToken |]

instance HasLomake AddToken where
    lomake _ =
        textFieldWithRegexp "Login" loginKleene :*
        dynEnumField "Policy" :*
        Nil

addTokenHandler :: LomakeRequest AddToken -> ReaderT (Login, Ctx f) IO (CommandResponse ())
addTokenHandler (LomakeRequest e) = ReaderT $ \(login, Ctx {..}) -> do
    -- we generate some token, user will regenerate it then
    base64T <- generateToken

    runLogT "add-token" ctxLogger $ do
        logInfoI "adding token for $user with $policy" e

        -- audit
        now <- currentTime
        void $ safePoolExecute ctxPostgresPool
            "INSERT INTO proxyapp.auditlog (username, created, message) VALUES (?, ?, ?);"
            (login, now, textShow e)

        -- execute action
        void $ safePoolExecute ctxPostgresPool insertQuery 
            (addTokenLogin e, base64T, login, addTokenPolicy e)

        -- ok: reload
        return CommandResponseReload
  where
    insertQuery = fromString $ unwords
        [ "INSERT INTO proxyapp.credentials"
        , "  (username, passtext, createdby, policyname, usertype)"
        , "VALUES"
        , "  (?, ?, ?, ?, 'user')"
        ]
