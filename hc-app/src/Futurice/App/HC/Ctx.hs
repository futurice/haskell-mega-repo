module Futurice.App.HC.Ctx where

import Prelude ()
import Futurice.Prelude

import Futurice.App.HC.Config

data Ctx = Ctx
    { ctxConfig  :: Config
    , ctxLogger  :: Logger
    , ctxManager :: Manager
    , ctxSecret  :: ByteString
    }
