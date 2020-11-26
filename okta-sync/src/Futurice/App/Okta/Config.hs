{-# LANGUAGE DataKinds #-}
module Futurice.App.Okta.Config where

import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified FUM.Types.Login as FUM
import qualified Okta            as O
import qualified Personio        as P

data Config = Config
    { cfgMockUser           :: !(Maybe FUM.Login)
    , cfgAccessGroups       :: ![O.GroupName]
    , cfgIntegrationsCfg    :: !(IntegrationsConfig '[ ServOK, ServPE, ServPK, ServPO ])
    , cfgGithubAppId        :: !O.OktaAppId
    , cfgAlwaysInPeakon     :: ![P.EmployeeId]
    , cfgIgnoreFromPersonio :: !(Set P.EmployeeId) -- Employee information we don't want to sync
    }

instance Configure Config where
  configure = Config
      <$> optionalAlt (envVar "MOCKUSER")
      <*> envVar "ACCESS_GROUPS"
      <*> configure
      <*> envVar "GITHUBAPPID"
      <*> envVar "ALWAYS_IN_PEAKON"
      <*> envVar "IGNORE_FROM_PERSONIO"

instance HasOktaGithubId Config where
    oktaGithubId = cfgGithubAppId
