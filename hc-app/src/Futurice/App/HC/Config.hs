{-# LANGUAGE DataKinds #-}
module Futurice.App.HC.Config (
    Config(..),
    ) where

import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified FUM.Types.Login     as FUM

data Config = Config
    { cfgIntegrationsCfg       :: !(IntegrationsConfig '[I, Proxy, Proxy, Proxy, Proxy, I])
    , cfgMockUser              :: !(Maybe FUM.Login)
    }

instance Configure Config where
    configure = Config
        <$> configure
        <*> optionalAlt (envVar "MOCKUSER")
