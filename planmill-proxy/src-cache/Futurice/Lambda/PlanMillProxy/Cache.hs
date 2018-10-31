{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
-- | This lambda updates everything except timereports and capacities
module Futurice.Lambda.PlanMillProxy.Cache (
    planMillProxyCacheLambda,
    ) where

import Data.Aeson         (FromJSON, Value, object, (.=))
import Data.Binary.Tagged (HasSemanticVersion, HasStructuralInfo, taggedEncode)
import Data.Constraint    (Dict (..))
import Data.Time          (addDays)
import Futurice.EnvConfig
import Futurice.Lambda
import Futurice.Postgres
       (ConnectInfo, HasPostgresPool, createPostgresPool, safePoolExecute,
       safePoolExecute_, safePoolQuery)
import Futurice.Prelude
import PlanMill.Worker    (Workers, submitPlanMill, workers)
import Prelude ()

import qualified Database.PostgreSQL.Simple as Postgres
import qualified PlanMill                   as PM
import qualified PlanMill.Types.Query       as PM

-- export handler
foreign export ccall planMillProxyCacheLambda :: AwsLambdaHandler

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
    impl _ _ Config {..} lgr mgr _ = do
        -- Setup
        pool <- createPostgresPool cfgPostgresConnInfo
        ws <- liftIO $ workers lgr mgr cfgPmCfg ["worker1", "worker2", "worker3"]
        now <- currentTime

        -- cleanup cache
        cleanupCache pool

        -- We want a UTC 02:00 point before `now`.
        let UTCTime today offset = now
        let stamp
              | offset < 7200 = UTCTime (addDays (-1) today) 7200
              | otherwise     = UTCTime today 7200

        logInfo "Fetching outdated queries" stamp
        qs <- safePoolQuery pool selectQuery (Only stamp)
        logInfo_ $ "Updating " <> textShow (length qs) <> " cache items"
        for_ qs $ \(Only (PM.SomeQuery q)) -> do
            res <- fetch ws pool q
            case res of
                Right () -> pure ()
                Left exc -> do
                    logAttention "Update failed" $ object [ "query" .= q, "exc" .= show exc ]
                    void $ safePoolExecute pool deleteQuery (Postgres.Only q)

    fetch :: HasPostgresPool ctx => Workers -> ctx -> PM.Query a -> LogT IO (Either SomeException ())
    fetch ws ctx q = case (binaryDict, semVerDict, structDict, nfdataDict, fromJsonDict) of
        (Dict, Dict, Dict, Dict, Dict) -> fetch' ws ctx q
      where
        binaryDict   = PM.queryDict (Proxy :: Proxy Binary) q
        semVerDict   = PM.queryDict (Proxy :: Proxy HasSemanticVersion) q
        structDict   = PM.queryDict (Proxy :: Proxy HasStructuralInfo) q
        nfdataDict   = PM.queryDict (Proxy :: Proxy NFData) q
        fromJsonDict = PM.queryDict (Proxy :: Proxy FromJSON) q

    fetch'
        :: (Binary a, NFData a, FromJSON a, HasStructuralInfo a, HasSemanticVersion a, HasPostgresPool ctx)
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

selectQuery :: Postgres.Query
selectQuery = fromString $ unwords
    [ "SELECT (query) FROM planmillproxy.cache"
    , "WHERE updated < ?"
    , "ORDER BY updated"
    , "LIMIT 200" -- cannot be much larger, we are slow to process some requsts
    , ";"
    ]

deleteQuery :: Postgres.Query
deleteQuery = "DELETE FROM planmillproxy.cache WHERE query = ?;"

storeInPostgres
    :: (Binary a, HasSemanticVersion a, HasStructuralInfo a, HasPostgresPool ctx)
    => ctx -> PM.Query a -> a -> LogT IO ()
storeInPostgres ctx q x = do
    -- -- logInfo_ $ "Storing in postgres" <> textShow q
    i <- safePoolExecute ctx postgresQuery (q, Postgres.Binary $ taggedEncode x)
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
