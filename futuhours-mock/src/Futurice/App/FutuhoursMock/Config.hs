module Futurice.App.FutuhoursMock.Config (
    Config(..),
    ) where

import Futurice.Prelude
import Prelude ()
import Futurice.EnvConfig

data Config = Config

instance Configure Config where
    configure = pure Config
