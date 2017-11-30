module Futurice.App.EmailProxy.Config where

import           Futurice.EnvConfig
import           Futurice.Prelude
import qualified Network.AWS        as AWS
import           Prelude ()

data Config = Config
    { cfgSesCreds :: !AWS.Credentials
    }
    deriving (Show)

instance Configure Config where
    configure = Config
        <$> envAwsCredentials "SES_"
