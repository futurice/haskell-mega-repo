module Futurice.App.OktaProxy.Config where

import Futurice.EnvConfig
import Futurice.Prelude
import Prelude ()

import qualified FUM.Types.GroupName as FUM
import qualified FUM.Types.Login     as FUM

data Config = Config
    { cfgMockUser           :: !(Maybe FUM.Login)
    }

instance Configure Config where
  configure = Config
      <$> optionalAlt (envVar "MOCKUSER")
