{-# LANGUAGE DataKinds #-}
module Futurice.App.Schedule.Config where

import Database.PostgreSQL.Simple (ConnectInfo)
import FUM.Types.Login            (Login)
import Futurice.EnvConfig
import Futurice.Integrations      (IntegrationsConfig, ServPE)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Schedule.Types

data Config = Config
    { cfgPostgesConnInfo    :: !ConnectInfo
    , cfgIntegrationsConfig :: !(IntegrationsConfig '[ ServPE ])
    , cfgMockUser           :: !(Maybe Login)
    , cfgCalendar           :: ![Calendar]
    }

instance Configure Config where
    configure = Config
        <$> envConnectInfo
        <*> configure
        <*> optionalAlt (envVar "MOCKUSER")
        <*> envVar "SCHEDULE_CALENDAR"
