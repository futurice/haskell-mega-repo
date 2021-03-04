{-# LANGUAGE DataKinds #-}
module Futurice.App.OktaProxy.Config where

import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified FUM.Types.Login as FUM

data Config = Config
    { cfgMockUser           :: !(Maybe FUM.Login)
    , cfgIntegrationsCfg    :: !(IntegrationsConfig '[ ServOK, ServPE ])
    }

instance Configure Config where
  configure = Config
      <$> optionalAlt (envVar "MOCKUSER")
      <*> configure
