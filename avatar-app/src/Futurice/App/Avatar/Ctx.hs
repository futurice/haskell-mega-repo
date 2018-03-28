module Futurice.App.Avatar.Ctx where

import Futurice.Integrations
import Futurice.Prelude
import Futurice.Servant
import Prelude ()

import Futurice.App.Avatar.Config

data Ctx = Ctx
    { ctxCache   :: !Cache
    , ctxLogger  :: !Logger
    , ctxManager :: !Manager
    , ctxConfig  :: !Config
    }
