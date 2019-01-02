module Futurice.App.Futuroom.Config where

import Futurice.EnvConfig
import Futurice.Prelude
import Prelude ()

data Config = Config
    { cfgGooglePrivateKey   :: !ByteString
    , cfgGoogleClientId     :: !Text
    , cfgGoogleClientEmail  :: !Text
    , cfgGooglePrivateKeyId :: !Text
    , cfgServiceAccountUser :: !Text
    }

instance Configure Config where
    configure = Config
        <$> envVar "GOOGLE_PRIVATE_KEY"
        <*> envVar "GOOGLE_CLIENT_ID"
        <*> envVar "GOOGLE_CLIENT_EMAIL"
        <*> envVar "GOOGLE_PRIVATE_KEY_ID"
        <*> envVar "SERVICE_ACCOUNT_USER"
