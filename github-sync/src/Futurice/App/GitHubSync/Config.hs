{-# LANGUAGE DataKinds #-}
module Futurice.App.GitHubSync.Config (
    Config (..),
    Pinned (..),
    ) where

import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()
import Text.Regex.Applicative (RE, match, psym, sym)

import qualified GitHub as GH

data Config = Config
    { cfgIntegrationsConfig :: !(IntegrationsConfig '[Proxy, Proxy, Proxy, I, Proxy, I]) -- TODO
    , cfgAuth               :: !GH.Auth
    , cfgOrganisationName   :: !(GH.Name GH.Organization)
    , cfgPinnedUsers        :: !Pinned
    }

instance Configure Config where
    configure = Config
        <$> configure
        <*> envVar "GH_AUTH_TOKEN"
        <*> envVar "GH_ORG"
        <*> envVar "GITHUBSYNC_PINNEDGHUSERS"

newtype Pinned = Pin { unPin :: [GH.Name GH.User] }

instance FromEnvVar Pinned where
    fromEnvVar = match pinnedRegex

pinnedRegex :: RE Char Pinned
pinnedRegex = fmap Pin $ pure []
    <|> liftA2 (:) single (many $ sym ',' *> single)
  where
    single = GH.mkUserName . view packed <$> some (psym (`notElem` ", "))
