module Futurice.App.OktaProxy.Ctx where

import Futurice.Prelude
import Futurice.Servant (Cache)
import Prelude ()

import Futurice.App.OktaProxy.Config

data Ctx = Ctx
    { ctxConfig  :: !Config
    , ctxLogger  :: !Logger
    , ctxManager :: !Manager
    , ctxCache   :: !Cache
    }
