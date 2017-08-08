module Futurice.App.Checklist.Config (
    Config(..),
    ) where

import Database.PostgreSQL.Simple (ConnectInfo)
import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified FUM

data Config = Config
    { cfgMockUser           :: !(Maybe FUM.Login)
    , cfgPostgresConnInfo   :: !ConnectInfo
    , cfgIntegrationsCfg    :: !(IntegrationsConfig Proxy I Proxy Proxy I)
    -- ACL Groups
    , cfgFumITGroup         :: !FUM.GroupName
    , cfgFumHRGroup         :: !FUM.GroupName
    , cfgFumSupervisorGroup :: !FUM.GroupName
    }

instance Configure Config where
    configure = Config
        <$> optionalAlt (envVar "MOCKUSER")
        <*> envConnectInfo
        <*> configure
        <*> envVar "FUM_IT_GROUP"
        <*> envVar "FUM_HR_GROUP"
        <*> envVar "FUM_SUPERVISOR_GROUP"
