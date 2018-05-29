{-# LANGUAGE DataKinds #-}
module Futurice.App.PersonioProxy.Config where

import Futurice.EnvConfig
import Futurice.Postgres  (ConnectInfo)
import Futurice.Prelude
import Futurice.Time
import Prelude ()

import qualified Personio

data Config = Config
    { cfgPersonioCfg      :: !Personio.Cfg
    , cfgPostgresConnInfo :: !ConnectInfo
    , cfgUpdateInterval   :: !(NDT 'Minutes Int)
    }

instance Configure Config where
    configure = Config
        <$> configure
        <*> envConnectInfo
        <*> fmap (max 3) (envVarWithDefault "UPDATE_INTERVAL" 5)
