{-# LANGUAGE DataKinds #-}
module Futurice.App.Contacts.Config (
    Config,
    ) where

import Prelude ()
import Futurice.Prelude
import Futurice.Integrations

type Config = IntegrationsConfig '[I, I, Proxy, I, I, I]
