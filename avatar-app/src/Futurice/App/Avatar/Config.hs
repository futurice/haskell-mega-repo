{-# LANGUAGE DataKinds #-}
module Futurice.App.Avatar.Config where

import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Postgres (ConnectInfo)
import Futurice.Prelude
import Prelude ()
import Servant.Client        (BaseUrl)

import qualified Network.AWS as AWS

data Config = Config
    { cfgIntegrationCfg   :: !(IntegrationsConfig '[Proxy, I, Proxy, Proxy, Proxy, Proxy])
    , cfgS3Bucket         :: !Text
    , cfgAwsCredentials   :: !AWS.Credentials
    , cfgSisosotaBaseurl  :: !BaseUrl
    , cfgPostgresConnInfo :: !ConnectInfo
    }

instance Configure Config where
    configure = Config
        <$> configure
        <*> envVar "S3_BUCKET"
        <*> envAwsCredentials "AWS_"
        <*> envVar "SISOSOTA_BASEURL"
        <*> envConnectInfo
