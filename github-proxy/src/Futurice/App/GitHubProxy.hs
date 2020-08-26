{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
module Futurice.App.GitHubProxy (defaultMain) where

import Futurice.Postgres (createPostgresPool)
import Futurice.Prelude
import Prelude ()

import Futurice.Periocron
import Futurice.Servant
import Servant

-- PlanmillProxy modules
import Futurice.App.GitHubProxy.API
import Futurice.App.GitHubProxy.Config (Config (..))
import Futurice.App.GitHubProxy.Logic  (cleanupCache, haxlEndpoint, updateCache)
import Futurice.App.GitHubProxy.Types  (Ctx (..))

server :: Ctx -> Server GitHubProxyAPI
server ctx = pure "Try /swagger-ui/"
    :<|> liftIO . haxlEndpoint ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService       .~ GithubProxyService
    & serverDescription   .~ "Make faster (and cached) queries to GitHub"
    & serverColour        .~ (Proxy :: Proxy ('FutuAccent 'AF6 'AC1))
    & serverApp githubProxyApi .~ server
    & serverEnvPfx        .~ "GITHUBPROXY"
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
    makeCtx (Config auth connectionInfo) logger _mgr cache _mq = do
        postgresPool <- createPostgresPool connectionInfo

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
