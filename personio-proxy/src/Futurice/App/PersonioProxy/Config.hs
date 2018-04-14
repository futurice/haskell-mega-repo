{-# LANGUAGE DataKinds #-}
module Futurice.App.PersonioProxy.Config where

import Futurice.EnvConfig
import Futurice.Prelude
import Futurice.Time
import Prelude ()

import qualified Personio

data Config = Config
    { cfgPersonioCfg    :: !Personio.Cfg
    , cfgUpdateInterval :: !(NDT 'Minutes Int)
    }

instance Configure Config where
    configure = Config
        <$> configure
        <*> fmap (max 3) (envVarWithDefault "UPDATE_INTERVAL" 5)
