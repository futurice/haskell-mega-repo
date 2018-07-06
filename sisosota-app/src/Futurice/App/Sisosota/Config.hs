{-# LANGUAGE DataKinds #-}
module Futurice.App.Sisosota.Config (
    Config(..),
    ) where

import Futurice.EnvConfig
import Futurice.Prelude
import Prelude ()

import qualified Network.AWS as AWS

data Config = Config
    { cfgS3Bucket       :: !Text
    , cfgAwsCredentials :: !AWS.Credentials
    }

instance Configure Config where
    configure = Config
        <$> envVar "S3_BUCKET"
        <*> envAwsCredentials "AWS_"
