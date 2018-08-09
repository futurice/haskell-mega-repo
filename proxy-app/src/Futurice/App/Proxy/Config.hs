{-# LANGUAGE TemplateHaskell #-}
module Futurice.App.Proxy.Config where

import Database.PostgreSQL.Simple (ConnectInfo)
import Futurice.EnvConfig
import Futurice.Prelude
import Prelude ()
import Servant.Client             (BaseUrl)

data Config = Config
    { cfgPostgresConnInfo     :: !ConnectInfo
    , _cfgAvatarBaseurl        :: !BaseUrl
    , _cfgReportsAppBaseurl    :: !BaseUrl
    , _cfgFumCarbonBaseurl     :: !BaseUrl
    , _cfgPlanmillProxyBaseurl :: !BaseUrl
    , _cfgGithubProxyBaseurl   :: !BaseUrl
    , _cfgPersonioProxyBaseurl :: !BaseUrl
    , _cfgPowerBaseurl         :: !BaseUrl
    , _cfgContactsApiBaseurl   :: !BaseUrl
    }

makeLenses ''Config

instance Configure Config where
    configure = Config
        <$> envConnectInfo
        <*> envVar "AVATAR_BASEURL"
        <*> envVar "REPORTSAPP_BASEURL"
        <*> envVar "FUMCARBON_BASEURL"
        <*> envVar "PLANMILLPROXY_BASEURL"
        <*> envVar "GITHUBPROXY_BASEURL"
        <*> envVar "PERSONIOPROXY_BASEURL"
        <*> envVar "POWER_BASEURL"
        <*> envVar "CONTACTSAPI_BASEURL"
