module Futurice.App.Futuroom.Config where

import Futurice.EnvConfig
import Futurice.Prelude
import Prelude ()

import qualified Google as G

data Config = Config
    { cfgGoogleConfig :: !G.GoogleCredentials
    }

instance Configure Config where
    configure = Config
        <$> configure
