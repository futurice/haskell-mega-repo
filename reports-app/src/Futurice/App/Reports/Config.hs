{-# LANGUAGE DataKinds #-}
module Futurice.App.Reports.Config (
    Config(..),
    ) where

import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

newtype Config = Config
    { cfgIntegrationsCfg :: IntegrationsConfig '[I, I, Proxy, I, I, I]
    }

instance Configure Config where
    configure = Config
        <$> configure
