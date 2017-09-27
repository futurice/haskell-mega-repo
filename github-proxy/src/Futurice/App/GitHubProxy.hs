{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.GitHubProxy (defaultMain) where

import Prelude ()
import Futurice.Prelude

import Data.Pool          (createPool)
import Futurice.Periocron
import Futurice.Servant
import Servant

import qualified Database.PostgreSQL.Simple as Postgres

-- PlanmillProxy modules
import Futurice.App.GitHubProxy.API
import Futurice.App.GitHubProxy.Config (Config (..))
import Futurice.App.GitHubProxy.Logic
       (cleanupCache, haxlEndpoint, updateCache)
import Futurice.App.GitHubProxy.Types  (Ctx (..))

server :: Ctx -> Server GitHubProxyAPI 
server ctx = pure "Try /swagger-ui/"
    :<|> liftIO . haxlEndpoint ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName          .~ "GitHub Proxy"
    & serverDescription   .~ "Make faster (and cached) queries to GitHub"
    & serverColour        .~ (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    & serverMiddleware    .~ liftFuturiceMiddleware logStdoutDev
    & serverApp githubProxyApi .~ server
    & serverEnvPfx        .~ "GITHUBPROXY"
  where
    makeCtx :: Config -> Logger -> Cache -> IO (Ctx, [Job])
    makeCtx (Config auth connectionInfo) logger cache = do
        postgresPool <- createPool
            (Postgres.connect connectionInfo)
            Postgres.close
            1 10 5

        let ctx = Ctx
                { ctxCache        = cache
                , ctxGitHubAuth   = auth
                , ctxPostgresPool = postgresPool
                , ctxLogger       = logger
                }

        let jobs =
                -- See every 5 minutes, if there's something to update in cache
                [ mkJob "cache update"  (updateCache ctx)
                  $ shifted 10 $ every $ 5 * 60

                -- Cleanup cache every three hours
                , mkJob "cache cleanup" (cleanupCache ctx)
                  $ every $ 180 * 60
                ]

        pure (ctx, jobs)
