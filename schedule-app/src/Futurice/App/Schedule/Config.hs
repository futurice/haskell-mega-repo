{-# LANGUAGE DataKinds #-}
module Futurice.App.Schedule.Config where

import Database.PostgreSQL.Simple (ConnectInfo)
import FUM.Types.Login            (Login)
import Futurice.EnvConfig
import Futurice.Integrations      (IntegrationsConfig, ServPE)
import Futurice.Prelude
import Prelude ()

data Config = Config
    { cfgPostgesConnInfo    :: !ConnectInfo
    , cfgIntegrationsConfig :: !(IntegrationsConfig '[ ServPE ])
    , cfgMockUser           :: !(Maybe Login)
    }

instance Configure Config where
    configure = Config
        <$> envConnectInfo
        <*> configure
        <*> optionalAlt (envVar "MOCKUSER")
