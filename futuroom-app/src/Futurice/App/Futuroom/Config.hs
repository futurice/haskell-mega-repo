{-# LANGUAGE DataKinds #-}
module Futurice.App.Futuroom.Config where

import Futurice.EnvConfig
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

data Config = Config
    { cfgGoogleConfig :: !(IntegrationsConfig '[ ServGO ])
    }

instance Configure Config where
    configure = Config
        <$> configure
