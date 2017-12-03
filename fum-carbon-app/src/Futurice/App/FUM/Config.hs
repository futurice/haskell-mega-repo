{-# LANGUAGE DataKinds #-}
module Futurice.App.FUM.Config (
    Config(..),
    ) where

import Database.PostgreSQL.Simple (ConnectInfo)
import FUM.Types.Login            (Login)
import Futurice.EnvConfig
import Futurice.Integrations      (IntegrationsConfig)
import Futurice.Prelude
import Prelude ()

import qualified Personio

data Config = Config
    { cfgPostgresConnInfo   :: !ConnectInfo
    , cfgPersonioCfg        :: !(Personio.Cfg)
    , cfgIntegrationsConfig :: !(IntegrationsConfig '[Proxy, I, Proxy, Proxy, Proxy, Proxy])
    , cfgMockUser           :: !(Maybe Login)
    }

instance Configure Config where
    configure = Config
        <$> envConnectInfo
        <*> configure
        <*> configure
        <*> optionalAlt (envVar "MOCKUSER")
