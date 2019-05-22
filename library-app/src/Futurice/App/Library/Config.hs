{-# LANGUAGE DataKinds #-}
module Futurice.App.Library.Config (
    Config(..),
    ) where

import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Postgres
import Futurice.Prelude
import Prelude ()
import Servant.Client        (BaseUrl)

import qualified FUM.Types.GroupName as FUM
import qualified FUM.Types.Login     as FUM

data Config = Config
    { cfgMockUser           :: !(Maybe FUM.Login)
    , cfgSisosotaUrl        :: !Text
    , cfgEmailProxyUrl      :: !BaseUrl
    , cfgAccessGroups       :: ![FUM.GroupName]
    , cfgPostgresConnInfo   :: !ConnectInfo
    , cfgIntegrationsCfg    :: !(IntegrationsConfig '[ ServFUM6, ServPE ])
    }

instance Configure Config where
    configure = Config
        <$> optionalAlt (envVar "MOCKUSER")
        <*> envVar "SISOSOTA_BASEURL"
        <*> envVar "EMAILPROXY_BASEURL"
        <*> envVar "REMINDER_ACCESS_GROUPS"
        <*> envConnectInfo
        <*> configure
