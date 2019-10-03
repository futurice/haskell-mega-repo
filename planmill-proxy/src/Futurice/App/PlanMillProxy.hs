{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.PlanMillProxy (defaultMain) where

import Data.Pool            (createPool)
import Futurice.Periocron
import Futurice.Prelude
import Futurice.Servant
import PlanMill.Types.Query (SomeQuery (..))
import PlanMill.Worker      (workers)
import Prelude ()
import Servant

import qualified Database.PostgreSQL.Simple as Postgres

-- PlanmillProxy modules
import Futurice.App.PlanMillProxy.API
import Futurice.App.PlanMillProxy.Charts
import Futurice.App.PlanMillProxy.Config               (Config (..))
import Futurice.App.PlanMillProxy.Logic
       (haxlEndpoint, statsEndpoint)
import Futurice.App.PlanMillProxy.Logic.UpdateAbsences
import Futurice.App.PlanMillProxy.Types                (Ctx (..))

import qualified PlanMill.Queries as PMQ


server :: Ctx -> Server PlanMillProxyAPI
server ctx = pure "Try /swagger-ui/"
    :<|> liftIO . haxlEndpoint ctx
    :<|> liftIO (statsEndpoint ctx)
    :<|> liftIO (timereportsAgeDistr ctx)

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService       .~ PlanmillProxyService
    & serverDescription   .~ "Make faster queries to PlanMill"
    & serverColour        .~ (Proxy :: Proxy ('FutuAccent 'AF6 'AC1))
    & serverApp planmillProxyApi .~ server
    & serverEnvPfx        .~ "PLANMILLPROXY"
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
    makeCtx (Config cfg connectionInfo) logger mgr cache mq = do
        postgresPool <- createPool
            (Postgres.connect connectionInfo)
            Postgres.close
            2 (10 :: NominalDiffTime) 20 -- stripes, ttl, resources
        ws <- workers logger mgr cfg ["worker1", "worker2", "worker3"]
        let ctx = Ctx
                { ctxCache        = cache
                , ctxPlanmillCfg  = cfg
                , ctxPostgresPool = postgresPool
                , ctxLogger       = logger
                , ctxWorkers      = ws
                }
        let jobs =
                [ mkJob "have to be warm" (updateWarmRequests ctx)
                  $ shifted (2 * 60) $ every $ 30 * 60
                ]

        void $ forEachMessage mq $ \msg -> case msg of
          AbsenceUpdatePing -> void $ updateAbsences ctx
          _ -> pure ()

        pure (ctx, jobs)

-- | We'll keep some queries warm.
updateWarmRequests :: Ctx -> IO ()
updateWarmRequests ctx = void $ haxlEndpoint ctx
    [ SomeQuery PMQ.usersQuery
    , SomeQuery PMQ.absencesQuery
    ]
