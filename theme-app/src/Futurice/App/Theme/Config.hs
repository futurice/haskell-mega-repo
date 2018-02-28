module Futurice.App.Theme.Config (
    Config(..),
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.EnvConfig

-- TODO: we could have mock user here!
data Config = Config deriving Show

instance Configure Config where
    configure = pure Config
