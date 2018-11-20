{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.Lambda.PlanMillProxy.Timereports (
    planMillProxyTimereportsLambda,
    ) where

import Data.Aeson                (Value, object, (.=))
import Data.Binary.Tagged        (taggedEncode)
import Data.Time                 (addDays)
import Futurice.EnvConfig
import Futurice.Lambda
import Futurice.Postgres
       (ConnectInfo, Connection, Pool, createPostgresPool, poolExecuteMany,
       safePoolExecute, safePoolQuery)
import Futurice.Prelude
import Numeric.Interval.NonEmpty (inf, sup, (...))
import PlanMill.Queries          (usersQuery)
import PlanMill.Worker
       (Workers, closeWorkers, submitPlanMillE, workers)
import Prelude ()

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Set                   as Set
import qualified Database.PostgreSQL.Simple as Postgres
import qualified PlanMill                   as PM
import qualified PlanMill.Types.Query       as PM

-- export handler
foreign export ccall planMillProxyTimereportsLambda :: AwsLambdaHandler

data Config = Config
    { cfgPmCfg            :: !PM.Cfg
    , cfgPostgresConnInfo :: !ConnectInfo
    }
  deriving (Show)

instance Configure Config where
    configure = Config
        <$> configure
        <*> envConnectInfo

planMillProxyTimereportsLambda :: AwsLambdaHandler
planMillProxyTimereportsLambda = makeAwsLambda impl where
    impl :: LambdaContext -> AWSEnv -> Config -> Logger -> Manager -> Value -> LogT IO ()
    impl lc _ Config {..} lgr mgr v = do
        -- Setup
        pool <- createPostgresPool cfgPostgresConnInfo
        ws <- liftIO $ workers lgr mgr cfgPmCfg ["worker1", "worker2", "worker3"]
        now <- currentTime

        if v == "without-timereports"
        then updateWithoutTimereports (lcRemainingTimeInMillis lc) pool ws
        else updateAllTimereports (lcRemainingTimeInMillis lc) pool ws now

        closeWorkers ws

-------------------------------------------------------------------------------
-- Intervals
-------------------------------------------------------------------------------

-- TODO: this needs updating. Making it dependent on current day?
intervals :: NonEmpty (PM.Interval Day)
intervals =
    ($(mkDay "2019-01-01") ... $(mkDay "2019-12-31")) :|
    [ $(mkDay "2018-01-01") ... $(mkDay "2018-12-31")
    , $(mkDay "2017-01-01") ... $(mkDay "2017-12-31")
    , $(mkDay "2016-01-01") ... $(mkDay "2016-12-31")
    , $(mkDay "2015-01-01") ... $(mkDay "2015-12-31")
    ]

-------------------------------------------------------------------------------
-- Without timereports
-------------------------------------------------------------------------------

-- | Update timereports for people without any timereports
updateWithoutTimereports
    :: IO Int -> Pool Connection -> Workers -> LogT IO ()
updateWithoutTimereports remaining pool ws = for_ intervals $ \interval -> do
    let dayMin = inf interval
    let dayMax = sup interval

    logInfoI "Selecting timereports for users without any $min ... $max" $ object
        [ "min" .= dayMin
        , "max" .= dayMax
        ]

    res <- liftIO $ submitPlanMillE ws $ PM.queryToRequest usersQuery
    case res of
        Left exc -> logAttention "Exception" $ object [ "exc" .= show exc ]
        Right allUsers -> do
            let allUidsSet = Set.fromList $ allUsers ^.. traverse . PM.identifier

            uids <- Postgres.fromOnly <$$> safePoolQuery pool selectUsersQuery (dayMin, dayMax)
            let uidsSet = Set.fromList uids
            let uidsWithout = Set.difference allUidsSet uidsSet

            logInfoI "Users without timereports: $count in $min ... $max" $ object
                [ "uids"  .= uidsWithout
                , "count" .= length uidsWithout
                , "min"   .= dayMin
                , "max"   .= dayMax
                ]

            for_ uidsWithout (updateTimereportsForUser remaining pool ws dayMin dayMax)
  where
    selectUsersQuery :: Postgres.Query
    selectUsersQuery = fromString $ unwords $
        [ "SELECT uid FROM planmillproxy.timereports WHERE day >= ? AND day <= ? GROUP BY uid"
        ]

-------------------------------------------------------------------------------
-- With timereports
-------------------------------------------------------------------------------

-- | Update timereports.
updateAllTimereports
    :: IO Int -> Pool Connection -> Workers -> UTCTime -> LogT IO ()
updateAllTimereports remaining pool ws now = do
    logInfo_ "Updating timereports for users"

    -- We want a UTC 02:00 point before `now`.
    let UTCTime today offset = now
    let threshold = 7200
    let stamp
          | offset < threshold = UTCTime (addDays (-1) today) threshold
          | otherwise     = UTCTime today threshold

    logInfoI "Selecting users with timereports updated before $stamp"
        $ object [ "stamp" .= stamp ]

    -- Select uids with oldest updated time reports
    (uids, dayMin, dayMax) <- selectUids stamp intervals

    logInfoI "Updating timereports for users ($min ... $max)" $ object
            [ "uids" .= uids
            , "min"  .= dayMin
            , "max"  .= dayMax
            ]

    for_ uids (updateTimereportsForUser remaining pool ws dayMin dayMax)
  where
    selectUids :: UTCTime -> NonEmpty (PM.Interval Day) -> LogT IO ([PM.UserId], Day, Day)
    selectUids stamp (i :| is) = do
        let dayMin = inf i
        let dayMax = sup i
        uids <- Postgres.fromOnly
            <$$> safePoolQuery pool selectUsersQuery (dayMin, dayMax, stamp)
        if null uids
        then case is of
            (i' : is') -> selectUids stamp (i' :| is')
            []         -> return ([], dayMin, dayMax)
        else return (uids, dayMin, dayMax)

    selectUsersQuery :: Postgres.Query
    selectUsersQuery = fromString $ unwords $
        [ "SELECT u.uid FROM "
        , "(SELECT uid, MIN(updated) as updated FROM planmillproxy.timereports WHERE day >= ? AND day <= ? GROUP BY uid) AS u"
        , "WHERE updated < ?"
        , "ORDER BY u.updated ASC LIMIT 67"
        , ";"
        ]

-------------------------------------------------------------------------------
-- Update timereports of user
-------------------------------------------------------------------------------

updateTimereportsForUser :: IO Int -> Pool Connection -> Workers -> Day -> Day -> PM.UserId -> LogT IO ()
updateTimereportsForUser remaining pool ws dayMin dayMax uid = do
    remaining' <- liftIO remaining
    if remaining' < 35 * 1000  -- PlanMill make take 30 seconds to answer
    then logAttentionI "Aborting rest jobs: $remaining ms remaining" $ object [ "remaining" .= remaining' ]
    else do
        let interval = dayMin ... dayMax
        let q = PM.QueryTimereports (Just interval) uid

        -- Fetch timereports from planmill
        res <- liftIO $ submitPlanMillE ws $ PM.queryToRequest q

        case res of
            Left exc -> logAttention "Exception" $ object [ "exc" .= show exc ]
            Right tr -> do
                -- Check what timereports we have stored, remove ones not in planmill anymore
                let planmillTrids = Set.fromList (tr ^.. traverse . PM.identifier)
                postgresTrids <- toTrids <$> safePoolQuery pool selectQuery (uid, dayMin, dayMax)

                let notInPlanmill = Set.difference postgresTrids planmillTrids
                when (not $ Set.null notInPlanmill) $ do
                    let notInPlanmillCount = Set.size notInPlanmill
                    logInfo_ $
                        "Found " <> textShow notInPlanmillCount <>
                        " timereports not in planmill anymore"
                    i <- safePoolExecute pool deleteQuery
                        (Postgres.Only $ Postgres.In $ Set.toList notInPlanmill)
                    when (fromIntegral i /= notInPlanmillCount) $
                        logAttention_ $
                            "Deleted " <> textShow i <>
                            " out of " <> textShow notInPlanmillCount <> " timereports"

                -- Insert timereports
                liftIO $ void $ insertTimereports pool tr
  where
    selectQuery :: Postgres.Query
    selectQuery = fromString $ unwords $
        [ "SELECT trid FROM planmillproxy.timereports"
        , "WHERE uid = ?"
        , "AND day >= ? AND day <= ?"
        , ";"
        ]

    toTrids :: [Postgres.Only PM.TimereportId] -> Set PM.TimereportId
    toTrids = Set.fromList . map Postgres.fromOnly

    deleteQuery :: Postgres.Query
    deleteQuery = "DELETE FROM planmillproxy.timereports WHERE trid IN ?;"

-- Helper function to insert timereports
insertTimereports
    :: Foldable f
    => Pool Connection
    -> f PM.Timereport
    -> IO Int64
insertTimereports pool trs =
    poolExecuteMany pool insertQuery $ transformForInsert <$> toList trs
  where
    transformForInsert
        :: PM.Timereport
        -> (PM.TimereportId, PM.UserId, Day, Postgres.Binary BSL.ByteString)
    transformForInsert tr =
        ( tr ^. PM.identifier
        , PM.trPerson tr
        , PM.trStart tr
        , Postgres.Binary $ taggedEncode tr
        )

    insertQuery :: Postgres.Query
    insertQuery = fromString $ unwords $
        [ "INSERT INTO planmillproxy.timereports as tr (trid, uid, day, data)"
        , "VALUES (?, ?, ?, ?)"
        , "ON CONFLICT (trid) DO UPDATE"
        , "SET uid = EXCLUDED.uid, day = EXCLUDED.day, data = EXCLUDED.data, updated = now(), variance = random()"
        , "WHERE tr.trid = EXCLUDED.trid"
        , ";"
        ]
