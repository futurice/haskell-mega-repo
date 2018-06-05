module Futurice.App.Preferences.Ctx where

import Futurice.Prelude
import Prelude ()
import Futurice.Postgres      (Connection, HasPostgresPool (..), Pool)

data Ctx = Ctx
    { ctxLogger  :: !Logger
    , ctxPostgres            :: !(Pool Connection)
    }

instance HasPostgresPool Ctx where
    postgresPool = ctxPostgres
