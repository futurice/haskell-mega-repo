{-# LANGUAGE DataKinds #-}
module Futurice.App.FUM.Config (
    Config(..),
    ) where

import Database.PostgreSQL.Simple (ConnectInfo)
import FUM.Types.Login            (Login)
import Futurice.EnvConfig
import Futurice.Integrations      (IntegrationsConfig, ServFUM, ServPE)
import Futurice.Prelude
import Futurice.Signed            (SecretKey)
import Prelude ()

data Config = Config
    { cfgPostgresConnInfo   :: !ConnectInfo
    , cfgIntegrationsConfig :: !(IntegrationsConfig '[ ServFUM, ServPE ])
    , cfgMockUser           :: !(Maybe Login)
    , cfgSecretKey          :: !SecretKey
    }

instance Configure Config where
    configure = Config
        <$> envConnectInfo
        <*> configure
        <*> optionalAlt (envVar "MOCKUSER")
        <*> envVar "BASIC_SECRETKEY"
