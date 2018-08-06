{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.Lambda.PlanMillProxy.Timereports (
    planMillProxyTimereportsLambda,
    ) where

import Data.Aeson                (Value)
import Data.Binary.Tagged        (taggedEncode)
import Futurice.EnvConfig
import Futurice.Lambda
import Futurice.Postgres
       (ConnectInfo, Connection, Pool, createPostgresPool, poolExecuteMany,
       safePoolExecute, safePoolQuery, safePoolQuery_)
import Futurice.Prelude
import Numeric.Interval.NonEmpty ((...))
import PlanMill.Queries          (usersQuery)
import PlanMill.Worker           (Workers, submitPlanMill, workers)
import Prelude ()

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
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
    impl _ _ Config {..} lgr mgr v = do
        -- Setup
        pool <- createPostgresPool cfgPostgresConnInfo
        ws <- liftIO $ workers lgr mgr cfgPmCfg ["worker1", "worker2", "worker3"]
        now <- currentTime

        if v == "without-timereports"
        then updateWithoutTimereports pool ws
        else updateAllTimereports pool ws now

-------------------------------------------------------------------------------
-- Without timereports
-------------------------------------------------------------------------------

-- | Update timereports for people without any timereports
updateWithoutTimereports
    :: Pool Connection -> Workers -> LogT IO ()
updateWithoutTimereports pool ws = do
    logInfo_ $ "Selecting timereports for users without any"

    res <- liftIO $ tryDeep $ submitPlanMill ws $ PM.queryToRequest usersQuery
    for_ res $ \allUsers -> do
        let allUidsSet = Set.fromList $ allUsers ^.. traverse . PM.identifier

        uids <- Postgres.fromOnly <$$> safePoolQuery_ pool selectUsersQuery
        let uidsSet = Set.fromList uids

        for_ (Set.difference allUidsSet uidsSet) (updateTimereportsForUser pool ws)
  where
    selectUsersQuery :: Postgres.Query
    selectUsersQuery = fromString $ unwords $
        [ "SELECT uid FROM planmillproxy.timereports GROUP BY uid"
        ]

-------------------------------------------------------------------------------
-- With timereports
-------------------------------------------------------------------------------

-- | Update timereports.
updateAllTimereports
    :: Pool Connection -> Workers -> UTCTime -> LogT IO ()
updateAllTimereports pool ws now = do
    logInfo_ $ "Updating timereports for users"

    -- Select uids with oldest updated time reports
    uids <- Postgres.fromOnly <$$> safePoolQuery pool selectUsersQuery
        (Postgres.Only $ previousThreeThirty now)
    logInfo_ $ "Updating timereports for users: " <>
        T.intercalate ", " (textShow . getIdent <$> uids)

    for_ uids (updateTimereportsForUser pool ws)
  where
    getIdent (PM.Ident a) = a

    selectUsersQuery :: Postgres.Query
    selectUsersQuery = fromString $ unwords $
        [ "SELECT u.uid FROM "
        , "(SELECT uid, MIN(updated) as updated FROM planmillproxy.timereports GROUP BY uid) AS u"
        , "WHERE updated < ?"
        , "ORDER BY u.updated ASC LIMIT 67"
        , ";"
        ]

-------------------------------------------------------------------------------
-- Update timereports of user
-------------------------------------------------------------------------------

updateTimereportsForUser :: Pool Connection -> Workers -> PM.UserId -> LogT IO ()
updateTimereportsForUser pool ws  uid = do
    let interval = $(mkDay "2015-01-01") ... $(mkDay "2018-12-31")
    let q = PM.QueryTimereports (Just interval) uid

    -- Fetch timereports from planmill
    res <- liftIO $ tryDeep $ submitPlanMill ws $ PM.queryToRequest q

    for_ res $ \tr -> do
        -- Check what timereports we have stored, remove ones not in planmill anymore
        let planmillTrids = Set.fromList (tr ^.. traverse . PM.identifier)
        postgresTrids <- toTrids <$> safePoolQuery pool selectQuery (Postgres.Only uid)

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

-------------------------------------------------------------------------------
-- Intervals
-------------------------------------------------------------------------------

-- | Get previous @03:30@ (in @Europe/Helsinki@ timezone) from the given
-- timestamp.
previousThreeThirty :: UTCTime -> UTCTime
previousThreeThirty x
    | y < x     = y
    | otherwise = z
  where
    LocalTime d _ = utcToHelsinkiTime x
    tod = TimeOfDay 3 30 0
    y = helsinkiTimeToUtc (LocalTime d tod)
    z = helsinkiTimeToUtc (LocalTime (pred d) tod)
