module Futurice.App.GitHubSync.Ctx (Ctx (..)) where

import Futurice.Prelude
import Futurice.Postgres
import Prelude ()

import Futurice.App.GitHubSync.Config

data Ctx = Ctx
    { ctxConfig       :: !Config
    , ctxLogger       :: !Logger
    , ctxManager      :: !Manager
    , ctxPostgresPool :: !(Pool Connection)
    }
