{-# LANGUAGE DataKinds #-}
module Futurice.App.HC.Config (
    Config(..),
    ) where

import Futurice.Email        (Email)
import Futurice.EnvConfig
import Futurice.Integrations
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
    , cfgEarlyCaringCC         :: !(Maybe Email)
    }

instance Configure Config where
    configure = Config
        <$> configure
        <*> optionalAlt (envVar "MOCKUSER")
        <*> envVar "ACCESS_GROUPS"
        <*> envVar "EMAILPROXY_BASEURL"
        <*> optionalAlt (envVar "EARLYCARING_CC")

