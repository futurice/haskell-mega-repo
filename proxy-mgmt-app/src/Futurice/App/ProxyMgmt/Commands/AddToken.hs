{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DerivingVia       #-}
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
  deriving (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData, ToHtml) via (Enumica TokenUser)

instance TextEnum TokenUser where
    type TextEnumNames TokenUser =
        '[ "user"
         , "service"
         ]

data AddToken = AddToken
    { addTokenLogin  :: !UserName
    , addTokenPolicy :: !PolicyName
    , addTokenType   :: !TokenUser
    }
  deriving (Show, Typeable, GhcGeneric)
  deriving anyclass (SopGeneric, HasDatatypeInfo)
  deriving (ToJSON, FromJSON) via (Sopica AddToken)

instance HasLomake AddToken where
    lomake _ =
        textField "Login" :*
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
            (addTokenLogin e, base64T, login, addTokenPolicy e, textShow (addTokenType e), addTokenPolicy e)

        -- ok: reload
        return CommandResponseReload
  where
    insertQuery = fromString $ unwords
        [ "INSERT INTO proxyapp.credentials"
        , "  (username, passtext, createdby, policyname, usertype)"
        , "VALUES"
        , "  (?, ?, ?, ?, ?)"
        , "ON CONFLICT (username) DO UPDATE SET policyname = ?;"
        ]
