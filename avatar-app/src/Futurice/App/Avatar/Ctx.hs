module Futurice.App.Avatar.Ctx where

import Futurice.Prelude
import Futurice.Servant
import Futurice.Postgres
import Prelude ()

import qualified Network.AWS as AWS

import Futurice.App.Avatar.Config

data Ctx = Ctx
    { ctxCache        :: !Cache
    , ctxLogger       :: !Logger
    , ctxManager      :: !Manager
    , ctxConfig       :: !Config
    , ctxAwsEnv       :: !AWS.Env
    , ctxPostgresPool :: !(Pool Connection)
    }

instance HasPostgresPool Ctx where
    postgresPool = ctxPostgresPool
