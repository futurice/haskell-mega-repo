{-# LANGUAGE OverloadedStrings #-}
module Google.Types where

import Data.Aeson.Types
import Futurice.EnvConfig
import Futurice.Prelude
import Network.Google.Auth
import Prelude ()

import qualified Data.ByteString.Base64 as Base64

data GoogleCredentials = GoogleCredentials
    { clientId           :: !Text
    , clientEmail        :: !Text
    , privateKey         :: !Text
    , privateKeyId       :: !Text
    , serviceAccountUser :: !Text
    } deriving Show

instance Configure GoogleCredentials where
    configure = GoogleCredentials
        <$> envVar "GOOGLE_CLIENT_ID"
        <*> envVar "GOOGLE_CLIENT_EMAIL"
        <*> (decodeUtf8Lenient . Base64.decodeLenient <$> envVar "GOOGLE_PRIVATE_KEY")
        <*> envVar "GOOGLE_PRIVATE_KEY_ID"
        <*> envVar "SERVICE_ACCOUNT_USER"

data Cfg = Cfg
    { googleCredentials  :: !GoogleCredentials
    , manager            :: !Manager
    }

class HasGoogleCfg a where
    googleCfg :: Lens' a GoogleCredentials

instance HasGoogleCfg Cfg where
    googleCfg f cfg = fmap (\newCred -> cfg { googleCredentials = newCred }) (f (googleCredentials cfg))

class HasHttpManager a where
    httpManager :: Lens' a Manager

instance HasHttpManager Cfg where
    httpManager f cfg = fmap (\newm -> cfg { manager = newm }) (f (manager cfg))

toCredentialsJson :: GoogleCredentials -> Value
toCredentialsJson (GoogleCredentials cid email key keyid _) =  object [ "private_key" .= key, "client_id" .= cid, "client_email" .= email, "private_key_id" .= keyid ]

toCredentials :: GoogleCredentials -> Credentials s
toCredentials cred = case FromAccount <$> parseEither parseJSON (toCredentialsJson cred) of
  Left e -> error e
  Right c -> c
