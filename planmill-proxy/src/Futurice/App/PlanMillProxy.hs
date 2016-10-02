{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes            #-}
module Futurice.App.PlanMillProxy (defaultMain) where

import Futurice.Prelude
import Prelude ()

import Futurice.Periocron
import Data.Pool        (createPool)
import Futurice.Servant
import Servant

import qualified Database.PostgreSQL.Simple as Postgres

-- PlanmillProxy modules
import Futurice.App.PlanMillProxy.API
import Futurice.App.PlanMillProxy.Config (Config (..))
import Futurice.App.PlanMillProxy.Logic  (haxlEndpoint, updateCache, cleanupCache)
import Futurice.App.PlanMillProxy.Types  (Ctx (..))

server :: Ctx -> Server PlanMillProxyAPI
server ctx = pure "Try /swagger-ui/"
    :<|> liftIO . haxlEndpoint ctx

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName          .~ "Planmill Proxy"
    & serverDescription   .~ "Make faster queries to PlanMill"
    & serverColour        .~ (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    & serverMiddleware    .~ liftFuturiceMiddleware logStdoutDev
    & serverApp planmillProxyApi .~ server
  where
    makeCtx :: Config -> DynMapCache -> IO Ctx
    makeCtx (Config cfg connectionInfo logLevel _) cache = do
        postgresPool <- createPool
            (Postgres.connect connectionInfo)
            Postgres.close
            1 10 5
        let ctx = Ctx
                { ctxCache        = cache
                , ctxPlanmillCfg  = cfg
                , ctxPostgresPool = postgresPool
                , ctxLogLevel     = logLevel
                }
        let jobs =
                [ (Job "cache update"  $ updateCache ctx,  every 60)
                -- Cleanup cache every three hours
                , (Job "cache cleanup" $ cleanupCache ctx, every $ 180 * 60)
                ]
        _ <- spawnPeriocron (Options runStderrLoggingT' 60) jobs
        pure ctx
    runStderrLoggingT'
        :: forall a. (forall m. (Applicative m, MonadLogger m, MonadIO m) => m a)
        -> IO a
    runStderrLoggingT' x = runStderrLoggingT x
