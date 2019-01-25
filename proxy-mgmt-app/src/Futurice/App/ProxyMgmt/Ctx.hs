module Futurice.App.ProxyMgmt.Ctx (
    Ctx (..),
    DashdoCtx (..),
    ) where

import Dashdo.Servant                (DashdoAPI)
import Futurice.App.ProxyMgmt.Config
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant              (Cache)
import Prelude ()
import Servant                       (Server)

data Ctx = Ctx
    { ctxPostgresPool :: !(Pool Connection)
    , ctxConfig       :: !Config
    , ctxLogger       :: !Logger
    , ctxCache        :: !Cache
    , ctxManager      :: !Manager
    , ctxDashdoServer :: !(Server DashdoAPI)
    }

data DashdoCtx = DashdoCtx
    { dctxPostgresPool :: !(Pool Connection)
    , dctxConfig       :: !Config
    , dctxLogger       :: !Logger
    , dctxCache        :: !Cache
    , dctxManager      :: !Manager
    }