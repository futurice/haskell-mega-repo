module Futurice.App.Futuroom.Ctx where

import Futurice.Prelude
import Futurice.Servant (Cache)
import Prelude ()

import Futurice.App.Futuroom.Config

data Ctx = Ctx
    { ctxConfig  :: !Config
    , ctxLogger  :: !Logger
    , ctxManager :: !Manager
    , ctxCache   :: !Cache
    }
