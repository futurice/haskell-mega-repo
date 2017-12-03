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
    , cfgReposUrl              :: !Text
    , cfgMissingHoursContracts :: !(Set (PM.EnumValue PM.User "contractType"))
    }

instance Configure Config where
    configure = Config
        <$> configure
        <*> envVar "REPORTS_GH_REPOSURL" -- TODO: change to REPORTSAPP_GH_REPOSURL
        <*> envVar "MISSINGHOURS_CONTRACTS"
