{-# LANGUAGE DataKinds #-}
module Futurice.App.FlowdockProxy.Config (
    Config(..),
    ) where

import Futurice.EnvConfig
import Futurice.Prelude
import Prelude ()
import Futurice.Postgres  (ConnectInfo)
import Futurice.Integrations

data Config = Config
    { cfgIntegrationsConfig :: !(IntegrationsConfig '[ ServFD, ServFUM6, ServPE])
    , cfgPostgresConnInfo   :: !ConnectInfo
    }

instance Configure Config where
    configure = Config
        <$> configure
        <*> envConnectInfo

