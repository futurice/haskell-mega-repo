{-# LANGUAGE DataKinds #-}
module Futurice.App.Library.Config (
    Config(..),
    ) where

import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Postgres
import Futurice.Prelude
import Prelude ()

import qualified FUM.Types.Login as FUM

data Config = Config
    { cfgMockUser           :: !(Maybe FUM.Login)
    , cfgAmazonAccessKey    :: !Text
    , cfgAmazonAssociateTag :: !Text
    , cfgAmazonSecretKey    :: !Text
    , cfgSisosotaUrl        :: !Text
    , cfgPostgresConnInfo   :: !ConnectInfo
    , cfgIntegrationsCfg    :: !(IntegrationsConfig '[ ServPE ])
    }

instance Configure Config where
    configure = Config
        <$> optionalAlt (envVar "MOCKUSER")
        <*> envVar "AMAZON_ACCESSKEY"
        <*> envVar "AMAZON_ASSOCIATETAG"
        <*> envVar "AMAZON_SECRETKEY"
        <*> envVar "SISOSOTA_BASEURL"
        <*> envConnectInfo
        <*> configure
