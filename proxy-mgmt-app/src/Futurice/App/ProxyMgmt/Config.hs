{-# LANGUAGE DataKinds #-}
module Futurice.App.ProxyMgmt.Config (
    Config(..),
    ) where

import Database.PostgreSQL.Simple (ConnectInfo)
import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified FUM.Types.GroupName as FUM
import qualified FUM.Types.Login     as FUM

data Config = Config
    { cfgPostgresConnInfo   :: !ConnectInfo
    , cfgIntegrationsConfig :: !(IntegrationsConfig '[ ServFUM6 ])
    , cfgMockUser            :: !(Maybe FUM.Login)
    , cfgAccessGroup         :: !(FUM.GroupName)
    }

instance Configure Config where
    configure = Config
        <$> envConnectInfo
        <*> configure
        <*> optionalAlt (envVar "MOCKUSER")
        <*> envVar "ACCESS_GROUP"
