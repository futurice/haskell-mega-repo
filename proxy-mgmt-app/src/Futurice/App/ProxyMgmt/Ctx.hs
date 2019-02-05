module Futurice.App.ProxyMgmt.Ctx (
    Ctx (..),
    ) where

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
    }