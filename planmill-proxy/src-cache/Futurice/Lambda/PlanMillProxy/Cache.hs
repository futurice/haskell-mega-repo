{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- | This lambda updates everything except timereports and capacities
module Futurice.Lambda.PlanMillProxy.Cache (
    planMillProxyCacheLambda,
    ) where

import Data.Aeson                   (FromJSON, object, parseJSON, (.=))
import Data.Aeson.Types             (parseMaybe)
import Data.Binary.Tagged           (Structured, structuredEncode)
import Data.Constraint              (Dict (..))
import Data.Time                    (addDays)
import Futurice.EnvConfig
import Futurice.Lambda
import Futurice.Postgres
       (ConnectInfo, HasPostgresPool, createPostgresPool, safePoolExecute,
       safePoolExecute_)
import Futurice.Postgres.SqlBuilder
import Futurice.Prelude
import PlanMill.Worker              (Workers, submitPlanMill, withWorkers)
import Prelude ()

import qualified Database.PostgreSQL.Simple as Postgres
import qualified PlanMill                   as PM
import qualified PlanMill.Types.Query       as PM

data Config = Config
    { cfgPmCfg            :: !PM.Cfg
    , cfgPostgresConnInfo :: !ConnectInfo
    }
  deriving (Show)

instance Configure Config where
    configure = Config
        <$> configure
        <*> envConnectInfo

planMillProxyCacheLambda :: AwsLambdaHandler
planMillProxyCacheLambda = makeAwsLambda impl
  where
    impl :: LambdaContext -> AWSEnv -> Config -> Logger -> Manager -> Value -> LogT IO ()
    impl lc _ Config {..} lgr mgr value = do
        -- Setup
        pool <- createPostgresPool cfgPostgresConnInfo
        now <- currentTime

        -- cleanup cache
        cleanupCache pool

       -- We want a UTC 02:00 point before `now`.
        let UTCTime today offset = now
        let stamp
              | offset < 7200 = UTCTime (addDays (-1) today) 7200
              | otherwise     = UTCTime today 7200

        -- limit
        let limit = maybe 200 (max 1) $ parseMaybe parseJSON value

        withWorkers lgr mgr cfgPmCfg ["worker1", "worker2", "worker3"] $ \ws -> do
            logInfoI "Fetching outdated queries, limit $limit" $ object
                [ "older-than" .= stamp
                , "limit"      .= limit
                ]

            qs <- safePoolQueryM pool "planmillproxy" $  do
                tbl <- from_ "cache"
                fields_ tbl ["query"]
                where_ [ ecolumn_ tbl "updated", " < ", eparam_ stamp ]
                orderby_ tbl "updated" ASC
                limit_ limit

            logInfo_ $ "Updating " <> textShow (length qs) <> " cache items"

            void $ runExceptT $ for_ qs $ \(Only (PM.SomeQuery q)) -> do
                when (not $ isSlowQuery q) $ do
                    remaining <- liftIO $ lcRemainingTimeInMillis lc
                    if remaining < 35 * 1000  -- PlanMill may take 30 seconds to answer
                    then do
                        logAttentionI "Aborting rest jobs: $remaining ms remaining" $ object
                            [ "remaining" .= remaining
                            ]
                        throwError () -- stop the whole job here
                    else do
                        res <- lift $ fetch ws pool q
                        case res of
                            Right () -> pure ()
                            Left exc -> do
                                logAttention "Update failed" $ object
                                    [ "query" .= q
                                    , "exc"   .= show exc
                                    ]
                                void $ safePoolExecute pool deleteQuery (Postgres.Only q)

    fetch :: HasPostgresPool ctx => Workers -> ctx -> PM.Query a -> LogT IO (Either SomeException ())
    fetch ws ctx q = case (binaryDict, structDict, nfdataDict, fromJsonDict) of
        (Dict, Dict, Dict, Dict) -> fetch' ws ctx q
      where
        binaryDict   = PM.queryDict (Proxy :: Proxy Binary) q
        structDict   = PM.queryDict (Proxy :: Proxy Structured) q
        nfdataDict   = PM.queryDict (Proxy :: Proxy NFData) q
        fromJsonDict = PM.queryDict (Proxy :: Proxy FromJSON) q

    fetch'
        :: (Binary a, NFData a, FromJSON a, Structured a, HasPostgresPool ctx)
        => Workers -> ctx -> PM.Query a
        -> LogT IO (Either SomeException ())
    fetch' ws ctx q = do
        x <- liftIO $ tryDeep $ submitPlanMill ws $ PM.queryToRequest q
        case x of
            Left err -> return (Left err)
            Right x' -> do
                storeInPostgres ctx q x'
                return (Right ())

    cleanupCache ctx = do
        i <- safePoolExecute_ ctx cleanupQuery
        logInfo_ $  "cleaned up " <> textShow i <> " cache items"
      where
        cleanupQuery :: Postgres.Query
        cleanupQuery = fromString $ unwords
            [ "DELETE FROM planmillproxy.cache"
            , "WHERE viewed < -4" -- -4 makes data survive over the weekends
            , ";"
            ]
    isSlowQuery (PM.QueryPagedGet PM.QueryTagAbsence _ _) = True
    isSlowQuery (PM.QueryPagedGet PM.QueryTagAccount _ _) = True
    isSlowQuery _                                         = False

deleteQuery :: Postgres.Query
deleteQuery = "DELETE FROM planmillproxy.cache WHERE query = ?;"

storeInPostgres
    :: (Binary a, Structured a, HasPostgresPool ctx)
    => ctx -> PM.Query a -> a -> LogT IO ()
storeInPostgres ctx q x = do
    -- -- logInfo_ $ "Storing in postgres" <> textShow q
    i <- safePoolExecute ctx postgresQuery (q, Postgres.Binary $ structuredEncode x)
    when (i == 0) $
        logAttention_ $ "Storing in postgres failed: " <> textShow q
  where
    postgresQuery = fromString $ unwords
        [ "INSERT INTO planmillproxy.cache as c (query, data)"
        , "VALUES (?, ?)"
        , "ON CONFLICT (query) DO UPDATE"
        , "SET data = EXCLUDED.data,"
        , "    viewed = case WHEN c.viewed <= 0 THEN c.viewed-1 ELSE 0 END,"
        , "    updated = now(),"
        , "    variance = random()"
        , "WHERE c.query = EXCLUDED.query"
        , ";"
        ]
