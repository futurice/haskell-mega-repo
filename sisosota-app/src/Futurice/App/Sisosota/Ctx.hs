module Futurice.App.Sisosota.Ctx (
    Ctx (..),
    ) where

import Futurice.App.Sisosota.Config
import Futurice.Prelude
import Futurice.Servant             (Cache)
import Prelude ()

import qualified Network.AWS as AWS

data Ctx = Ctx
    { ctxConfig  :: !Config
    , ctxLogger  :: !Logger
    , ctxCache   :: !Cache
    , ctxManager :: !Manager
    , ctxAwsEnv  :: !AWS.Env
    }
