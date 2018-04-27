module Futurice.App.PlanMillProxy.Config (
    Config(..),
    ) where

import Futurice.EnvConfig
import Futurice.Postgres  (ConnectInfo)
import Futurice.Prelude
import PlanMill           (Cfg (..))
import Prelude ()

data Config = Config
    { cfgCfg              :: !Cfg
    , cfgPostgresConnInfo :: !ConnectInfo
    }
  deriving (Show)

instance Configure Config where
    configure = Config
        <$> configure
        <*> envConnectInfo
