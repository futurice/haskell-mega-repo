{-# LANGUAGE DataKinds #-}
module Futurice.App.Avatar.Config where

import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified Network.AWS as AWS

data Config = Config
    { cfgIntegrationCfg :: !(IntegrationsConfig '[ServFUM])
    , cfgS3Bucket       :: !Text
    , cfgAwsCredentials :: !AWS.Credentials
    }

instance Configure Config where
    configure = Config
        <$> configure
        <*> envVar "S3_BUCKET"
        <*> envAwsCredentials "AWS_"
