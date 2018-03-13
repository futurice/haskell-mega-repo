module Futurice.App.PersonioProxy.Config where

import Futurice.EnvConfig
import Futurice.Prelude
import Prelude ()

import qualified Personio

data Config = Config
    { cfgPersonioCfg :: !(Personio.Cfg)
    }

instance Configure Config where
    configure = Config
        <$> configure
