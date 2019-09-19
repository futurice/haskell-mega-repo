module Futurice.App.Okta.Ctx where

import Futurice.Prelude
import Futurice.Servant (Cache)
import Prelude ()

import Futurice.App.Okta.Config

data Ctx = Ctx
    { ctxConfig  :: !Config
    , ctxLogger  :: !Logger
    , ctxManager :: !Manager
    , ctxCache   :: !Cache
    }
