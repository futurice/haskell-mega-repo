{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Futurice.Lambda.PlanMillProxy.Capacity (
    planMillProxyCapacityLambda,
    ) where

import Data.Aeson         (Value, object, (.=))
import Data.Binary.Tagged (taggedEncode)
import Data.Time          (addDays, addUTCTime)
import Futurice.EnvConfig
import Futurice.Lambda
import Futurice.Postgres
       (ConnectInfo, Query, createPostgresPool, safePoolExecuteMany,
       safePoolQuery)
import Futurice.Prelude
import PlanMill.Worker    (submitPlanMill, workers)
import Prelude ()

import qualified Data.ByteString.Lazy       as BSL
import qualified Database.PostgreSQL.Simple as Postgres
import qualified PlanMill                   as PM
import qualified PlanMill.Types.Query       as PM

-- export handler
foreign export ccall planMillProxyCapacityLambda :: AwsLambdaHandler

data Config = Config
    { cfgPmCfg            :: !PM.Cfg
    , cfgPostgresConnInfo :: !ConnectInfo
    }
  deriving (Show)

instance Configure Config where
    configure = Config
        <$> configure
        <*> envConnectInfo

planMillProxyCapacityLambda :: AwsLambdaHandler
planMillProxyCapacityLambda = makeAwsLambda impl where
    impl :: LambdaContext -> AWSEnv -> Config -> Logger -> Manager -> Value -> LogT IO ()
    impl _ _ Config {..} lgr mgr _ = do
        -- Setup
        pool <- createPostgresPool cfgPostgresConnInfo
        ws <- liftIO $ workers lgr mgr cfgPmCfg ["worker1", "worker2", "worker3"]
        now <- currentTime
        today <- currentDay

        -- Find older than 12h capacities to update
        let stamp = addUTCTime (-43200) now
        logInfo "Selecting old data in cache" stamp
        res <- safePoolQuery pool selectQuery (Only stamp)

        for_ res $ \(pmUid, mi, ma) -> do
            let interval = mi PM.... max ma (addDays 30 today)
            logInfo "Updating employees capacities" $ object
                [ "pm-uid"   .= pmUid
                , "interval" .= interval
                ]
            ecapacities <- liftIO $ tryDeep $ submitPlanMill ws $ PM.queryToRequest $
                PM.QueryCapacities interval pmUid
            case ecapacities of
                Left err -> logAttention "PM failed" (show err)
                Right capacities -> do
                    logTrace "Got results from PM" (length capacities)
                    _ <- safePoolExecuteMany pool insertQuery $
                        transformForInsert pmUid capacities
                    return ()

transformForInsert
    :: PM.UserId
    -> PM.UserCapacities
    -> [(PM.UserId, Day, Postgres.Binary BSL.ByteString)]
transformForInsert uid = fmap f . toList
  where
    f uc = (uid, PM.userCapacityDate uc, Postgres.Binary $ taggedEncode uc)

selectQuery :: Query
selectQuery = fromString $ unwords $
    [ "SELECT uid, MIN(day) as min, MAX(day) as max"
    , "FROM planmillproxy.capacity"
    , "WHERE updated < ?"
    , "GROUP BY uid"
    , "LIMIT 100"
    , ";"
    ]

insertQuery :: Postgres.Query
insertQuery = fromString $ unwords $
    [ "INSERT INTO planmillproxy.capacity as c (uid, day, data)"
    , "VALUES (?, ?, ?)"
    , "ON CONFLICT (uid, day) DO UPDATE"
    , "SET  data = EXCLUDED.data, updated = current_timestamp"
    , "WHERE c.uid = EXCLUDED.uid AND c.day = EXCLUDED.day"
    ]
