{-# LANGUAGE DataKinds #-}
module Futurice.App.Avatar.Config where

import Prelude ()
import Futurice.Prelude
import Futurice.Integrations

-- import Futurice.EnvConfig

type Config = IntegrationsConfig '[Proxy, I, Proxy, Proxy, Proxy, Proxy]
