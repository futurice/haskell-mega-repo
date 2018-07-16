module Futurice.App.Proxy.Config (
    Config(..),
    ) where

import Prelude ()
import Futurice.Prelude
import Database.PostgreSQL.Simple (ConnectInfo)
import Futurice.EnvConfig
import Servant.Client             (BaseUrl)

data Config = Config
    { cfgPostgresConnInfo     :: !ConnectInfo
    , cfgReportsAppBaseurl    :: !BaseUrl
    , cfgFumCarbonBaseurl     :: !BaseUrl
    , cfgPlanmillProxyBaseurl :: !BaseUrl
    , cfgGithubProxyBaseurl   :: !BaseUrl
    , cfgPersonioProxyBaseurl :: !BaseUrl
    , cfgPowerBaseurl         :: !BaseUrl
    , cfgContactsApiBaseurl   :: !BaseUrl
    }

instance Configure Config where
    configure = Config
        <$> envConnectInfo
        <*> envVar "REPORTSAPP_BASEURL"
        <*> envVar "FUMCARBON_BASEURL"
        <*> envVar "PLANMILLPROXY_BASEURL"
        <*> envVar "GITHUBPROXY_BASEURL"
        <*> envVar "PERSONIOPROXY_BASEURL"
        <*> envVar "POWER_BASEURL"
        <*> envVar "CONTACTSAPI_BASEURL"
