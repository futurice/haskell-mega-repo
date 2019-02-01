{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.ProxyMgmt.Main (defaultMain) where

import Dashdo.Servant
import FUM.Types.Login           (Login)
import Futurice.FUM.MachineAPI   (FUM6 (..), fum6)
import Futurice.Integrations
import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Postgres         (createPostgresPool)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant
import Servant.Server.Generic

import qualified Data.Set                   as Set
import qualified Database.PostgreSQL.Simple as Postgres

-- ProxyMgmt modules
import Futurice.App.ProxyMgmt.API
import Futurice.App.ProxyMgmt.Config          (Config (..))
import Futurice.App.ProxyMgmt.Ctx
import Futurice.App.ProxyMgmt.Dashdo
import Futurice.App.ProxyMgmt.Pages.Audit
import Futurice.App.ProxyMgmt.Pages.Index
import Futurice.App.ProxyMgmt.Pages.Policies
import Futurice.App.ProxyMgmt.Pages.Tokens
import Futurice.App.ProxyMgmt.Pages.Reports
import Futurice.App.ProxyMgmt.Commands.RegenerateToken
import Futurice.App.ProxyMgmt.Commands.AddEndpoint
import Futurice.App.ProxyMgmt.Commands.AddToken
import Futurice.App.ProxyMgmt.Commands.RemoveEndpoint

server :: Ctx -> Server ProxyMgmtAPI
server ctx = genericServer $ ProxyMgmtRoutes
    { routeIndexPage          = \mfu -> nt False ctx mfu indexPageHandler
    , routeRegenerateOwnToken = \mfu -> nt False ctx mfu regenerateTokenHandler
    -- admin
    , routeTokensPage         = \mfu -> nt True  ctx mfu tokensPageHandler
    , routePoliciesPage       = \mfu -> nt True  ctx mfu policiesPageHandler
    , routeAuditPage          = \mfu -> nt True  ctx mfu auditPageHandler
    , routeReportsPage        = \mfu -> nt True  ctx mfu reportsPageHandler
    -- commands
    , routeAddEndpoint        = \mfu -> nt True ctx mfu . addEndpointHandler
    , routeRemoveEndpoint     = \mfu -> nt True ctx mfu . removeEndpointHandler
    , routeAddToken           = \mfu -> nt True ctx mfu . addTokenHandler
    -- charts
    , routeChartPerUser       = \mfu -> nt True ctx mfu $ chartHandler chartPerUser
    , routeChartPerEndpoint   = \mfu -> nt True ctx mfu $ chartHandler chartPerEndpoint
    , routeChartPerDay        = \mfu -> nt True ctx mfu $ chartHandler chartPerDay
    -- dashdo
    , routeDashdo             = ctxDashdoServer ctx
    }

-- Access control adding transformation
nt :: Bool -> Ctx -> Maybe Login -> ReaderT (Login, Ctx) IO a -> Handler a
nt requireAdmin ctx mfu handler = do
    now <- currentTime

    -- Access control
    fus <- liftIO $ cachedIO lgr cch 60 () $
        runIntegrations mgr lgr now (cfgIntegrationsConfig cfg) $
            fum6 $ FUMGroupEmployees (cfgAccessGroup cfg)

    case mfu <|> cfgMockUser cfg of
        Just fu | requireAdmin `implication` Set.member fu fus
             -> liftIO $ runReaderT handler (fu, ctx)
        _    -> throwError err403
  where
    cch = ctxCache ctx
    cfg = ctxConfig ctx
    lgr = ctxLogger ctx
    mgr = ctxManager ctx

    implication False _ = True
    implication True  x = x

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService          .~ ProxMgmtService
    & serverDescription      .~ "Audit log"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF6 'AC3))
    & serverApp proxyMgmtApi .~ server
    & serverEnvPfx           .~ "PROXYMGMT"
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
    makeCtx cfg@Config {..} lgr mgr cache _mq = do
        postgresPool <- createPostgresPool cfgPostgresConnInfo
        let ctx' = DashdoCtx postgresPool cfg lgr cache mgr
        dashdoServer <- makeDashdoServer ctx'
        pure (Ctx postgresPool cfg lgr cache mgr dashdoServer, [])
