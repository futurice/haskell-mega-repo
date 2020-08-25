{-# LANGUAGE DataKinds #-}
module Futurice.App.Checklist.Config (
    Config(..),
    ChecklistIntegrations,
    ) where

import Database.PostgreSQL.Simple (ConnectInfo)
import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified FUM.Types.GroupName as FUM
import qualified FUM.Types.Login     as FUM
import qualified Okta                as O

type ChecklistIntegrations = '[ ServFUM6, ServGH, ServOK, ServPE, ServPM ]

data Config = Config
    { cfgMockUser           :: !(Maybe FUM.Login)
    , cfgPostgresConnInfo   :: !ConnectInfo
    , cfgIntegrationsCfg    :: !(IntegrationsConfig ChecklistIntegrations)
    -- ACL Groups
    , cfgFumITGroup         :: !FUM.GroupName
    , cfgFumHRGroup         :: !FUM.GroupName
    , cfgFumSupervisorGroup :: !FUM.GroupName
    , cfgGithubAppId        :: !O.OktaAppId
    }

instance Configure Config where
    configure = Config
        <$> optionalAlt (envVar "MOCKUSER")
        <*> envConnectInfo
        <*> configure
        <*> envVar "FUM_IT_GROUP"
        <*> envVar "FUM_HR_GROUP"
        <*> envVar "FUM_SUPERVISOR_GROUP"
        <*> envVar "GITHUBAPPID"

instance HasOktaGithubId Config where
    oktaGithubId = cfgGithubAppId
