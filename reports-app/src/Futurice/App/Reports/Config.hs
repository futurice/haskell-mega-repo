{-# LANGUAGE DataKinds #-}
module Futurice.App.Reports.Config (
    Config(..),
    ) where

import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()
import Servant.Client        (BaseUrl)

data Config = Config
    { cfgIntegrationsCfg       :: !(IntegrationsConfig '[I, I, Proxy, I, I, I])
    , cfgEmailProxyBaseurl     :: !BaseUrl
    , cfgSmsProxyBaseurl       :: !BaseUrl
    , cfgPreferencesAppBaseurl :: !BaseUrl
    }

instance Configure Config where
    configure = Config
        <$> configure
        <*> envVar "EMAILPROXY_BASEURL"
        <*> envVar "SMSPROXY_BASEURL"
        <*> envVar "PREFERENCES_BASEURL"
