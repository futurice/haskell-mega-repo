{-# LANGUAGE DataKinds #-}
module Futurice.App.GitHubSync.Config (
    Config (..),
    Pinned (..),
    ) where

import Database.PostgreSQL.Simple (ConnectInfo)
import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()
import Text.Regex.Applicative     (RE, match, psym, sym)

import qualified FUM.Types.GroupName as FUM
import qualified FUM.Types.Login     as FUM
import qualified GitHub              as GH
import qualified Okta                as O

data Config = Config
    { cfgIntegrationsCfg  :: !(IntegrationsConfig '[ ServFUM6, ServGH, ServOK, ServPE ])
    , cfgAuth             :: !GH.Auth
    , cfgOrganisationName :: !(GH.Name GH.Organization)
    , cfgPinnedUsers      :: !Pinned
    , cfgMockUser         :: !(Maybe FUM.Login)
    , cfgAccessGroup      :: !FUM.GroupName
    , cfgPostgresConnInfo :: !ConnectInfo
    , cfgGithubOktaId     :: !O.OktaAppId
    }

instance Configure Config where
    configure = Config
        <$> configure
        <*> envVar "GH_AUTH_TOKEN"
        <*> envVar "GH_ORG"
        <*> envVar "GITHUBSYNC_PINNEDGHUSERS"
        <*> optionalAlt (envVar "MOCKUSER")
        <*> envVar "ACCESS_GROUP"
        <*> envConnectInfo
        <*> envVar "GITHUB_OKTA_GROUP_ID"

newtype Pinned = Pin { unPin :: [GH.Name GH.User] }

instance FromEnvVar Pinned where
    fromEnvVar = match pinnedRegex

pinnedRegex :: RE Char Pinned
pinnedRegex = fmap Pin $ pure []
    <|> liftA2 (:) single (many $ sym ',' *> single)
  where
    single = GH.mkUserName . view packed <$> some (psym (`notElem` ", "))
