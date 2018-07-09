module Futurice.App.Reports.Ctx (
    Ctx(..),
    ) where

import Dashdo.Servant    (DashdoAPI)
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant  (Cache)
import Prelude ()
import Servant           (Server)

import Futurice.App.Reports.Config

-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

data Ctx = Ctx
    { ctxCache        :: !Cache
    , ctxManager      :: !Manager
    , ctxLogger       :: !Logger
    , ctxConfig       :: !Config
    , ctxDashdo       :: !(Server DashdoAPI)
    , ctxPostgresPool :: !(Pool Connection)
    }

instance HasPostgresPool Ctx where
    postgresPool = ctxPostgresPool
