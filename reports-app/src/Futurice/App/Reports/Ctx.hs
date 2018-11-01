{-# LANGUAGE DataKinds #-}
module Futurice.App.Reports.Ctx (
    Ctx(..),
    runIntegrations',
    ) where

import Dashdo.Servant        (DashdoAPI)
import Futurice.Integrations
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant      (Cache)
import Prelude ()
import Servant               (Server)

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

-------------------------------------------------------------------------------
-- Run integrations
-------------------------------------------------------------------------------

runIntegrations' :: Ctx -> Integrations '[ ServFD, ServFUM, ServGH, ServPE, ServPM ] a -> IO a
runIntegrations' ctx m = do
    now <- currentTime
    runIntegrations mgr lgr now (cfgIntegrationsCfg cfg) m
  where
    mgr = ctxManager ctx
    lgr = ctxLogger ctx
    cfg = ctxConfig ctx
