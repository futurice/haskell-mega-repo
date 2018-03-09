{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.App.ProxyMgmt.RegenerateToken (regenerateTokenHandler) where

import Data.Aeson                 (object, (.=))
import Database.PostgreSQL.Simple (Only (..))
import FUM.Types.Login
import Futurice.Postgres
import Futurice.Prelude
import Prelude ()
import System.Entropy             (getEntropy)

import qualified Data.ByteString.Base64.URL as Base64
import qualified Data.Text                  as T

import Futurice.App.ProxyMgmt.Ctx
import Futurice.App.ProxyMgmt.Types

regenerateTokenHandler :: Ctx f -> ReaderT Login IO Text
regenerateTokenHandler Ctx {..} = ReaderT $ \login -> do
    bytes <- getEntropy 30
    now <- currentTime
    let base64 =  Base64.encode bytes
        base64T = decodeUtf8Lenient base64
        maskedT = T.take 6 base64T <> T.replicate 34 "*"

    runLogT "regenerateTokenHandler" ctxLogger $ do
        logInfo "regenerateToken" $ object
            [ "username" .= login
            , "token" .= maskedT
            ]

        void $ safePoolExecute ctxPostgresPool
            "INSERT INTO proxyapp.auditlog (username, created, message) VALUES (?, ?, ?);"
            (login, now, "regenerated token, new token " <> maskedT)

        -- TODO: check that audit log writing succeed?

        void $ safePoolExecute ctxPostgresPool
            "UPDATE proxyapp.credentials SET passtext = crypt(?, gen_salt('bf')) WHERE username = ?;"
            (base64T, login)

    return base64T
