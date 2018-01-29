{-# LANGUAGE DataKinds #-}
module Futurice.App.HC.Config (
    Config(..),
    ) where

import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified FUM.Types.GroupName as FUM
import qualified FUM.Types.Login     as FUM

data Config = Config
    { cfgIntegrationsCfg       :: !(IntegrationsConfig '[I, Proxy, I, Proxy, Proxy, I])
    , cfgMockUser              :: !(Maybe FUM.Login)
    , cfgAccessGroups          :: ![FUM.GroupName]
    }

instance Configure Config where
    configure = Config
        <$> configure
        <*> optionalAlt (envVar "MOCKUSER")
        <*> envVar "ACCESS_GROUPS"
