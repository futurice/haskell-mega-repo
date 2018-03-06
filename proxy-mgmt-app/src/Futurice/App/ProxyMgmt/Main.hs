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

import qualified Data.Set                   as Set
import qualified Database.PostgreSQL.Simple as Postgres

-- ProxyMgmt modules
import Futurice.App.ProxyMgmt.AdminPage
import Futurice.App.ProxyMgmt.API
import Futurice.App.ProxyMgmt.Config    (Config (..))
import Futurice.App.ProxyMgmt.Ctx
import Futurice.App.ProxyMgmt.Dashdo
import Futurice.App.ProxyMgmt.IndexPage
import Futurice.App.ProxyMgmt.RegenerateToken

server :: Ctx Identity -> Server ProxyMgmtAPI
server ctx =
    (\mfu -> nt False ctx mfu $ indexPageHandler ctx)
    :<|> (\mfu -> nt False ctx mfu $ regenerateTokenHandler ctx)
    :<|> (\mfu -> nt True ctx mfu $ adminPageHandler ctx)

-- Access control adding transformation
nt :: Bool -> Ctx f -> Maybe Login -> ReaderT Login IO a -> Handler a
nt requireAdmin ctx mfu handler = do
    now <- currentTime

    -- Access control
    fus <- liftIO $ cachedIO lgr cch 60 () $
        runIntegrations mgr lgr now (cfgIntegrationsConfig cfg) $
            fum6 $ FUMGroupEmployees (cfgAccessGroup cfg)

    case mfu <|> cfgMockUser cfg of
        Just fu | requireAdmin `implication` Set.member fu fus
             -> liftIO $ runReaderT handler fu
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
    & serverName             .~ "Prox Management"
    & serverDescription      .~ "Audit log"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF6 'AC3))
    & serverApp proxyMgmtApi .~ server
    & serverEnvPfx           .~ "PROXYMGMT"
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> IO (Ctx Identity, [Job])
    makeCtx cfg@Config {..} lgr mgr cache = do
        postgresPool <- createPostgresPool cfgPostgresConnInfo
        let ctx = Ctx postgresPool cfg lgr cache mgr Proxy
        dashdoServer <- makeDashdoServer ctx
        pure (ctx { ctxDashdoServer = Identity dashdoServer }, [])
