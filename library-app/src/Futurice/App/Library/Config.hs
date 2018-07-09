{-# LANGUAGE DataKinds #-}
module Futurice.App.Library.Config (
    Config(..),
    ) where

import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Postgres
import Futurice.Prelude
import Prelude ()

import qualified FUM.Types.Login as FUM
import qualified Personio

data Config = Config
    { cfgMockUser         :: !(Maybe FUM.Login)
    , cfgPostgresConnInfo :: !ConnectInfo
    , cfgIntegrationsCfg  :: !(IntegrationsConfig '[Proxy, Proxy, Proxy, Proxy, Proxy, I])
    }

instance Configure Config where
    configure = Config
        <$> optionalAlt (envVar "MOCKUSER")
        <*> envConnectInfo
        <*> configure
