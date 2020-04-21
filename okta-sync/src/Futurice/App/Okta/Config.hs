{-# LANGUAGE DataKinds #-}
module Futurice.App.Okta.Config where

import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified FUM.Types.GroupName as FUM
import qualified FUM.Types.Login     as FUM

data Config = Config
    { cfgMockUser           :: !(Maybe FUM.Login)
    , cfgAccessGroups       :: ![FUM.GroupName]
    , cfgIntegrationsCfg    :: !(IntegrationsConfig '[ ServFUM6, ServOK, ServPE, ServPO ])
    , cfgGithubAppId        :: !Text
    }

instance Configure Config where
  configure = Config
      <$> optionalAlt (envVar "MOCKUSER")
      <*> envVar "ACCESS_GROUPS"
      <*> configure
      <*> envVar "GITHUBAPPID"

instance HasOktaGithubId Config where
    oktaGithubId = cfgGithubAppId
