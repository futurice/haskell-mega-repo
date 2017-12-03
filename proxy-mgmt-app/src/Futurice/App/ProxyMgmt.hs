{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.ProxyMgmt (defaultMain) where

import Futurice.Prelude
import Prelude ()
import Futurice.Integrations
import Futurice.Postgres (createPostgresPool)
import Futurice.Servant
import Servant
import Dashdo.Servant
import Futurice.FUM.MachineAPI (FUM6 (..), fum6)

import qualified Data.Set        as Set
import qualified Database.PostgreSQL.Simple as Postgres

-- ProxyMgmt modules
import Futurice.App.ProxyMgmt.API
import Futurice.App.ProxyMgmt.Config (Config (..))
import Futurice.App.ProxyMgmt.Dashdo
import Futurice.App.ProxyMgmt.Ctx

server :: Ctx Identity -> Server ProxyMgmtAPI
server ctx mfu = hoistServer (Proxy :: Proxy DashdoAPI) nt (runIdentity (ctxDashdoServer ctx))
  where
    -- Access control adding transformation
    nt :: Handler a -> Handler a
    nt handler = do
        now <- currentTime

        -- Access control
        fus <- liftIO $ cachedIO lgr cch 60 () $
            runIntegrations mgr lgr now (cfgIntegrationsConfig cfg) $
                fum6 $ FUMGroupEmployees (cfgAccessGroup cfg)

        case mfu <|> cfgMockUser cfg of
            Just fu | Set.member fu fus -> handler
            _                           -> throwError err403

    cch = ctxCache ctx
    cfg = ctxConfig ctx
    lgr = ctxLogger ctx
    mgr = ctxManager ctx

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
