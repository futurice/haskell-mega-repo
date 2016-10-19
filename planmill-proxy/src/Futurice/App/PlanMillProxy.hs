{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.PlanMillProxy (defaultMain) where

import Futurice.Prelude
import Prelude ()

import Data.Pool          (createPool)
import Futurice.Periocron
import Futurice.Servant
import Servant

import qualified Database.PostgreSQL.Simple as Postgres

-- PlanmillProxy modules
import Futurice.App.PlanMillProxy.API
<<<<<<< HEAD
import Futurice.App.PlanMillProxy.Config (Config (..))
import Futurice.App.PlanMillProxy.Logic  (haxlEndpoint, updateCache, cleanupCache)
=======
import Futurice.App.PlanMillProxy.Config (Config (..), getConfig)
import Futurice.App.PlanMillProxy.Logic
       (cleanupCache, haxlEndpoint, updateCache, updateCapacities,
       updateAllTimereports, updateWithoutTimereports)
>>>>>>> 6f7f394... Planmill proxy additions
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
                -- See every 5 minutes, if there's something to update in cache
                [ ( Job "cache update"  $ updateCache ctx
                  , shifted (3 * 60) $ every $ 5 * 60
                  )
                -- Cleanup cache every three hours
                , ( Job "cache cleanup" $ cleanupCache ctx
                  , every $ 180 * 60
                  )
                -- Update capacities once in a while
                , ( Job "capacities update" $ updateCapacities ctx
                  , tail $ every $ 15 * 60
                  )
                {-
                -- Update recent timereports often
                , ( Job "timereports update" $ updateRecentTimereports ctx
                  , shifted (0 * 60) $ every $ 15 * 60
                  )
                -}
                -- Update timereports 
                , ( Job "update timereports" $ updateAllTimereports ctx
                  , shifted (5 * 60) $ every $ 45 * 60 -- TODO: see how often it should be run
                  )

                , ( Job "update without timereports" $ updateWithoutTimereports ctx
                  , shifted (10 * 60) $ every $ 2 * 60 * 60
                  )
                ]

        -- Spawn periocron, polling each minute
        _ <- spawnPeriocron (Options runStderrLoggingT' 60) jobs

        pure ctx
    runStderrLoggingT'
        :: forall a. (forall m. (Applicative m, MonadLogger m, MonadIO m) => m a)
        -> IO a
    runStderrLoggingT' x = runStderrLoggingT x
