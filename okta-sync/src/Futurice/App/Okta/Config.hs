{-# LANGUAGE DataKinds #-}
module Futurice.App.Okta.Config where

import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified FUM.Types.GroupName as FUM
import qualified FUM.Types.Login     as FUM
import qualified Okta                as O
import qualified Personio            as P

data Config = Config
    { cfgMockUser           :: !(Maybe FUM.Login)
    , cfgAccessGroups       :: ![FUM.GroupName]
    , cfgIntegrationsCfg    :: !(IntegrationsConfig '[ ServFUM6, ServOK, ServPE, ServPK, ServPO ])
    , cfgGithubAppId        :: !O.OktaAppId
    , cfgAlwaysInPeakon     :: ![P.EmployeeId]
    }

instance Configure Config where
  configure = Config
      <$> optionalAlt (envVar "MOCKUSER")
      <*> envVar "ACCESS_GROUPS"
      <*> configure
      <*> envVar "GITHUBAPPID"
      <*> envVar "ALWAYS_IN_PEAKON"

instance HasOktaGithubId Config where
    oktaGithubId = cfgGithubAppId
