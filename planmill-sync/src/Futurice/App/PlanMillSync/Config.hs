{-# LANGUAGE DataKinds #-}
module Futurice.App.PlanMillSync.Config (
    Config (..),
    ) where

import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified FUM.Types.GroupName as FUM
import qualified FUM.Types.Login     as FUM
import qualified PlanMill            as PM

data Config = Config
    { cfgIntegrationsConfig  :: !(IntegrationsConfig '[ ServFUM, ServPE, ServPM ])
    , cfgIntegrationsConfig2 :: !(IntegrationsConfig '[ ServFUM6 ])
    , cfgMockUser            :: !(Maybe FUM.Login)
    , cfgAccessGroup         :: !FUM.GroupName
    , cfgPlanMillCfg         :: !PM.Cfg
    }

instance Configure Config where
    configure = Config
        <$> configure
        <*> configure
        <*> optionalAlt (envVar "MOCKUSER")
        <*> envVar "ACCESS_GROUP"
        <*> configure
