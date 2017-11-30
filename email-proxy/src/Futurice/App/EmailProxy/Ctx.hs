module Futurice.App.EmailProxy.Ctx where

import Futurice.Prelude
import Prelude ()

import Futurice.App.EmailProxy.Config

import qualified Network.AWS as AWS

data Ctx = Ctx
    { ctxLogger  :: !Logger
    , ctxConfig  :: !Config
    , ctxAwsEnv  :: !AWS.Env
    }
