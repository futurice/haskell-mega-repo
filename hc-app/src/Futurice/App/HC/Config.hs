{-# LANGUAGE DataKinds #-}
module Futurice.App.HC.Config (
    Config(..),
    ) where

import Futurice.Email        (Email)
import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Postgres
import Futurice.Prelude
import Prelude ()
import Servant.Client        (BaseUrl)

import qualified FUM.Types.GroupName as FUM
import qualified FUM.Types.Login     as FUM

data Config = Config
    { cfgIntegrationsCfg       :: !(IntegrationsConfig '[ ServFUM6, ServPE, ServPM ])
    , cfgMockUser              :: !(Maybe FUM.Login)
    , cfgAccessGroups          :: ![FUM.GroupName]
    , cfgEmailProxyBaseurl     :: !BaseUrl
    , cfgEarlyCaringCC         :: !(NonEmpty Email)
    , cfgPeopleManager         :: !FUM.Login
    , cfgPostgresConnInfo      :: !ConnectInfo
    }

instance Configure Config where
    configure = Config
        <$> configure
        <*> optionalAlt (envVar "MOCKUSER")
        <*> envVar "ACCESS_GROUPS"
        <*> envVar "EMAILPROXY_BASEURL"
        <*> envVar "EARLYCARING_CC"
        <*> envVar "PEOPLE_MANAGER"
        <*> envConnectInfo
