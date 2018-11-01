{-# LANGUAGE DataKinds #-}
module Futurice.App.Reports.Config (
    Config(..),
    ) where

import Database.PostgreSQL.Simple (ConnectInfo)
import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()
import Servant.Client             (BaseUrl)

data Config = Config
    { cfgIntegrationsCfg       :: !(IntegrationsConfig '[ ServFD, ServFUM, ServGH, ServPE, ServPM ])
    , cfgEmailProxyBaseurl     :: !BaseUrl
    , cfgSmsProxyBaseurl       :: !BaseUrl
    , cfgPreferencesAppBaseurl :: !BaseUrl
    , cfgPostgresConnInfo      :: !ConnectInfo
    , cfgPostgresConnInfoInv   :: !ConnectInfo
    }

instance Configure Config where
    configure = Config
        <$> configure
        <*> envVar "EMAILPROXY_BASEURL"
        <*> envVar "SMSPROXY_BASEURL"
        <*> envVar "PREFERENCES_BASEURL"
        <*> envConnectInfo
        <*> envConnectInfo' "INVENTORY_"
