{-# LANGUAGE DataKinds #-}
module Futurice.App.Reports.Config (
    Config(..),
    ) where

import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified PlanMill           as PM

data Config = Config
    { cfgIntegrationsCfg       :: !(IntegrationsConfig '[I, I, Proxy, I, I, I])
    , cfgMissingHoursContracts :: !(Set (PM.EnumValue PM.User "contractType"))
    }

instance Configure Config where
    configure = Config
        <$> configure
        <*> envVar "MISSINGHOURS_CONTRACTS"
