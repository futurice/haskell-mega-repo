{-# LANGUAGE DataKinds #-}
module Futurice.App.FUM.Config (
    Config(..),
    ) where

import Database.PostgreSQL.Simple (ConnectInfo)
import FUM.Types.Login            (Login)
import Futurice.EnvConfig
import Futurice.Integrations      (IntegrationsConfig, ServFUM)
import Futurice.Prelude
import Futurice.Signed            (SecretKey)
import Prelude ()

import qualified Personio

data Config = Config
    { cfgPostgresConnInfo   :: !ConnectInfo
    , cfgPersonioCfg        :: !Personio.Cfg
    , cfgIntegrationsConfig :: !(IntegrationsConfig '[ ServFUM ])
    , cfgMockUser           :: !(Maybe Login)
    , cfgSecretKey          :: !SecretKey
    }

instance Configure Config where
    configure = Config
        <$> envConnectInfo
        <*> configure
        <*> configure
        <*> optionalAlt (envVar "MOCKUSER")
        <*> envVar "BASIC_SECRETKEY"
