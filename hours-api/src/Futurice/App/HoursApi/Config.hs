{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
module Futurice.App.HoursApi.Config (
    Config(..),
    ) where

import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified FUM.Types.Login as FUM
import qualified PlanMill        as PM

data Config = Config
    { cfgIntegrationsCfg :: !(IntegrationsConfig '[I, Proxy, Proxy, Proxy, Proxy, I])
    , cfgPlanmillCfg     :: !PM.Cfg
    , cfgMockUser        :: !(Maybe FUM.Login)
    }

instance Configure Config where
    configure = Config
        <$> configure
        <*> configure
        <*> optionalAlt (envVar "MOCKUSER")
