{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE TemplateHaskell    #-}
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

import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres

data TokenUser
    = User
    | Service
  deriving (Show, Typeable, GhcGeneric, Enum, Bounded)
  deriving anyclass (SopGeneric, HasDatatypeInfo)

instance TextEnum TokenUser where
    type TextEnumNames TokenUser =
        '[ "user"
         , "service"
         ]

deriveVia [t| ToJSON TokenUser             `Via` Enumica TokenUser |]
deriveVia [t| FromJSON TokenUser           `Via` Enumica TokenUser |]
deriveVia [t| ToHttpApiData TokenUser      `Via` Enumica TokenUser |]
deriveVia [t| FromHttpApiData TokenUser    `Via` Enumica TokenUser |]
deriveVia [t| ToHtml TokenUser             `Via` Enumica TokenUser |]

data AddToken = AddToken
    { addTokenLogin  :: !Login
    , addTokenPolicy :: !PolicyName
    , addTokenType   :: !TokenUser
    }
  deriving (Show, Typeable, GhcGeneric)
  deriving anyclass (SopGeneric, HasDatatypeInfo)
  deriving (ToJSON, FromJSON) via (Sopica AddToken)

instance HasLomake AddToken where
    lomake _ =
        textFieldWithRegexp "Login" loginKleene :*
        dynEnumField "Policy" :*
        enumField "User type" enumToText :*
        Nil

addTokenHandler :: LomakeRequest AddToken -> ReaderT (Login, Ctx) IO (CommandResponse ())
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
