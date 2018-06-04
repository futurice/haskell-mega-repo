module Futurice.App.Preferences.Config where

import Futurice.EnvConfig
import Futurice.Postgres  (ConnectInfo)
import Futurice.Prelude
import Prelude ()

newtype Config = Config
    { cfgPostgresConnInfo :: ConnectInfo
    }
    deriving (Show)

instance Configure Config where
    configure = Config
        <$> envConnectInfo
