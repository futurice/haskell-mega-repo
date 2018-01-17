{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RecordWildCards #-}
module Futurice.App.HoursApi.Config (
    Config(..),
    ) where

import Database.PostgreSQL.Simple (ConnectInfo)
import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified FUM
import qualified PlanMill as PM

data Config = Config
    { cfgIntegrationsCfg :: !(IntegrationsConfig '[I, I, Proxy, Proxy, Proxy, Proxy])
    , cfgPlanmillCfg     :: !PM.Cfg
    , cfgMockUser        :: !(Maybe FUM.Login)
    , cfgPostgresConnInfo  :: !ConnectInfo
    }

instance Configure Config where
    configure = Config
        <$> configure
        <*> configure
        <*> optionalAlt (envVar "MOCKUSER")
        <*> envConnectInfo
