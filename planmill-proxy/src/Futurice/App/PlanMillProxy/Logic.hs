{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Futurice.App.PlanMillProxy.Logic (
    -- * Endpoint
    haxlEndpoint,
    -- * Generic cache
    updateCache,
    cleanupCache,
    -- * Capacities
    updateCapacities,
    -- * Timereports
    updateAllTimereports,
    updateWithoutTimereports,
    ) where

import Futurice.Prelude
import Prelude ()

import Control.Monad.Catch              (handle)
import Control.Monad.Logger             (LoggingT, filterLogger)
import Control.Monad.PlanMill           (planmillQuery)
import Data.Binary.Tagged
       (HasSemanticVersion, HasStructuralInfo, taggedDecode, taggedEncode)
import Data.Constraint
import Data.Pool                        (withResource)
import Futurice.App.PlanMillProxy.H
import Futurice.App.PlanMillProxy.Types (Ctx (..))
import Futurice.Servant
       (CachePolicy (..), DynMapCache, genCachedIO)
import Numeric.Interval.NonEmpty        (inf, sup, (...))
import PlanMill.Queries                 (usersQuery)
import PlanMill.Types                   (Cfg)
<<<<<<< HEAD
import PlanMill.Types.Query             (Query, SomeQuery (..), SomeResponse (..), queryDict)
=======
import PlanMill.Types.Query             (Query (..), SomeQuery (..), queryDict)
>>>>>>> 6f7f394... Planmill proxy additions

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.HashMap.Strict        as HM
import qualified Data.Set                   as Set
import qualified Data.Vector                as V
import qualified Data.Text                as T
import qualified Database.PostgreSQL.Simple as Postgres
import qualified PlanMill                   as PM

-------------------------------------------------------------------------------
-- Intervals
-------------------------------------------------------------------------------

genericAge :: String
genericAge = "'60 minutes'"

capacityAge :: String
capacityAge = "'6 hours'"

-------------------------------------------------------------------------------
-- Type synonyms
-------------------------------------------------------------------------------

-- | /TODO/ Store 'SomeResponse' in the database?
type CacheLookup = HashMap SomeQuery BSL.ByteString

lookupCache :: [(SomeQuery, Postgres.Binary BSL.ByteString)] -> CacheLookup
lookupCache ps = HM.fromList (Postgres.fromBinary <$$> ps)

type LIO = LoggingT IO

runLIO :: Ctx -> (Postgres.Connection -> LIO a) -> IO a
runLIO ctx f = withResource (ctxPostgresPool ctx) $ \conn -> runLoggingT' ctx $ f conn

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

-- | The haxl endpoint. We take list of 'Query', and return list of results
haxlEndpoint :: Ctx -> [SomeQuery] -> IO [Either Text SomeResponse]
haxlEndpoint ctx qs = runLIO ctx $ \conn -> do
    _ <- handleSqlError 0 $ Postgres.execute conn viewQuery postgresQs
    cacheResult <- liftIO $ lookupCache <$> Postgres.query conn selectQuery postgresQs
    $(logInfo) $ "Found "
        <> textShow (HM.size cacheResult) <> "/"
        <> textShow (length primitiveQs) <> " in postgres"
    traverse (fetch cacheResult conn) qs
  where
    primitiveQs = filter isPrimitive qs
    postgresQs  = Postgres.Only . Postgres.In $ primitiveQs

    -- We handle timereports and capacities specially
    isPrimitive (SomeQuery (QueryCapacities _ _))  = False
    isPrimitive (SomeQuery (QueryTimereports _ _)) = False
    isPrimitive _                                  = True

    -- Fetch provides context for fetch', i.e. this is boilerplate :(
    fetch
        :: CacheLookup -> Postgres.Connection -> SomeQuery
<<<<<<< HEAD
        -> LIO (Either Text SomeResponse)
=======
        -> LIO (Either Text ByteString)
    fetch _cacheResult conn (SomeQuery (QueryCapacities i u)) =
        taggedEncode <$$> selectCapacities ctx conn u i
    fetch _cacheResult conn (SomeQuery (QueryTimereports i u)) =
        taggedEncode <$$> selectTimereports ctx conn u i
>>>>>>> 6f7f394... Planmill proxy additions
    fetch cacheResult conn (SomeQuery q) =
        case (binaryDict, semVerDict, structDict, nfdataDict) of
            (Dict, Dict, Dict, Dict) -> fetch' cacheResult conn q
      where
        binaryDict = queryDict (Proxy :: Proxy Binary) q
        semVerDict = queryDict (Proxy :: Proxy HasSemanticVersion) q
        structDict = queryDict (Proxy :: Proxy HasStructuralInfo) q
        nfdataDict = queryDict (Proxy :: Proxy NFData) q

    fetch'
        :: (NFData a, Binary a, HasSemanticVersion a, HasStructuralInfo a)
        => CacheLookup  -> Postgres.Connection -> Query a
        -> LIO (Either Text SomeResponse)
    fetch' cacheResult conn q = case HM.lookup (SomeQuery q) cacheResult of
        Just bs -> do
            x <- liftIO $ tryDeep (return . MkSomeResponse q . id' q . taggedDecode $ bs)
            case x of
                Right y -> return (Right y)
                Left exc -> do
                    $(logWarn) $ "Borked cache content for " <> textShow q
                    _ <- handleSqlError 0 $
                        Postgres.execute conn deleteQuery (Postgres.Only q)
                    return $ Left $ show exc ^. packed
        Nothing -> MkSomeResponse q <$$> fetch'' conn q

    -- We use proxy to force the type
    id' :: proxy a -> a -> a
    id' _ = id

    -- Fetch and store
    fetch''
        :: (NFData a, Binary a, HasSemanticVersion a, HasStructuralInfo a)
        => Postgres.Connection -> Query a -> LIO (Either Text a)
    fetch'' conn q = do
        res <- liftIO $ tryDeep $ runLoggingT' ctx $ do
            x <- fetchFromPlanMill (ctxCache ctx) (ctxPlanmillCfg ctx) q
            storeInPostgres conn q x
            pure x
        return $ first (\x -> show x ^. packed) res

    -- Used to update viewed counters
    viewQuery :: Postgres.Query
    viewQuery = "UPDATE planmillproxy.cache SET viewed = viewed + 1 WHERE query in ?;"

    -- Used to delete invalid items (cannot decode)
    deleteQuery :: Postgres.Query
    deleteQuery = "DELETE FROM planmillproxy.cache WHERE query = ?;"

    -- Select multiple items
    selectQuery :: Postgres.Query
    selectQuery = fromString $ unwords $
        [ "SELECT query, data FROM planmillproxy.cache"
        , "WHERE query in ?"
        , ";"
        ]

-- | Update cache, we look what's viewed the most and update these entries.
-- This means that we never delete items from cache
updateCache :: Ctx -> IO ()
updateCache ctx = runLIO ctx $ \conn -> do
    qs <- handleSqlError [] $ Postgres.query_ conn selectQuery
    $(logInfo) $ "Updating " <> textShow (length qs) <> " cache items"
    for_ qs $ \(Postgres.Only (SomeQuery q)) -> fetch conn q
  where
    fetch :: Postgres.Connection -> Query a -> LIO (Either SomeException ())
    fetch conn q =
        case (binaryDict, semVerDict, structDict, nfdataDict) of
            (Dict, Dict, Dict, Dict) -> fetch' conn q
      where
        binaryDict = queryDict (Proxy :: Proxy Binary) q
        semVerDict = queryDict (Proxy :: Proxy HasSemanticVersion) q
        structDict = queryDict (Proxy :: Proxy HasStructuralInfo) q
        nfdataDict = queryDict (Proxy :: Proxy NFData) q

    fetch'
      :: (Binary a, HasStructuralInfo a, HasSemanticVersion a)
      => Postgres.Connection -> Query a -> LIO (Either SomeException ())
    fetch' conn q = liftIO $ tryDeep $ runLoggingT' ctx $ do
        x <- fetchFromPlanMill (ctxCache ctx) (ctxPlanmillCfg ctx) q
        storeInPostgres conn q x

    -- Fetch queries which are old enough, and viewed at least once
    selectQuery :: Postgres.Query
    selectQuery = fromString $ unwords $
        [ "SELECT (query) FROM planmillproxy.cache"
        , "WHERE current_timestamp - updated > (" ++ genericAge ++ " :: interval) * (1 + variance) AND viewed > 0"
        , "ORDER BY viewed"
        , "LIMIT 1000"
        , ";"
        ]

-- | Cleanup cache
cleanupCache :: Ctx -> IO ()
cleanupCache ctx = runLIO ctx $ \conn -> do
    i <- handleSqlError 0 $ Postgres.execute_ conn cleanupQuery
    $(logInfo) $  "cleaned up " <> textShow i <> " cache items"
  where
    cleanupQuery :: Postgres.Query
    cleanupQuery = fromString $ unwords $
        [ "DELETE FROM planmillproxy.cache"
        , "WHERE current_timestamp - updated > '24 hours' AND viewed <= 0"
        , ";"
        ]

storeInPostgres
    :: (Binary a, HasSemanticVersion a, HasStructuralInfo a)
    => Postgres.Connection -> Query a -> a -> LIO ()
storeInPostgres conn q x = do
    i <- handleSqlError 0 $
        Postgres.execute conn postgresQuery (q, Postgres.Binary $ taggedEncode x)
    when (i == 0) $
        $(logWarn) $ "Storing in postgres failed: " <> show q ^. packed
  where
    postgresQuery = fromString $ unwords $
        [ "INSERT INTO planmillproxy.cache as c (query, data)"
        , "VALUES (?, ?)"
        , "ON CONFLICT (query) DO UPDATE"
        , "SET data = EXCLUDED.data, viewed = 0, updated = now(), variance = random()"
        , "WHERE c.query = EXCLUDED.query"
        , ";"
        ]

-------------------------------------------------------------------------------
-- Time reports
-------------------------------------------------------------------------------

-- | Select timereports
selectTimereports
    :: Ctx -> Postgres.Connection
    -> PM.UserId -> Maybe (PM.Interval Day) -> LIO (Either Text PM.Timereports)
selectTimereports _ctx conn uid minterval = do
    res <- handleSqlError [] $ case minterval of
        Nothing       -> Postgres.query conn selectQueryWithoutInterval (Postgres.Only uid)
        Just interval -> Postgres.query conn selectQueryWithInterval (uid, inf interval, sup interval)
    res' <- liftIO $ tryDeep $ return $ V.fromList $ map selectTransform res
    return $ first (\x -> show x ^. packed) res'
  where
    selectTransform
        :: Postgres.Only (Postgres.Binary BSL.ByteString)
        -> PM.Timereport
    selectTransform (Postgres.Only (Postgres.Binary bs)) = taggedDecode bs

    selectQueryWithInterval :: Postgres.Query
    selectQueryWithInterval = fromString $ unwords $
        [ "SELECT (data) FROM planmillproxy.timereports"
        , "WHERE uid = ? AND day >= ? AND day <= ?"
        , ";"
        ]

    selectQueryWithoutInterval :: Postgres.Query
    selectQueryWithoutInterval = fromString $ unwords $
        [ "SELECT (data) FROM planmillproxy.timereports"
        , "WHERE uid = ?"
        , ";"
        ]

-- | Update timereports for people without any timereports
updateWithoutTimereports
    :: Ctx -> IO ()
updateWithoutTimereports ctx = runLIO ctx $ \conn -> do
    $(logInfo) $ "Selecting timereports for users without any"

    allUsers <- fetchFromPlanMill (ctxCache ctx) (ctxPlanmillCfg ctx) usersQuery
    let allUidsSet = Set.fromList $ allUsers ^.. traverse . PM.identifier

    uids <- Postgres.fromOnly <$$> handleSqlError [] (Postgres.query_ conn selectUsersQuery)
    let uidsSet = Set.fromList uids

    for_ (Set.difference allUidsSet uidsSet) (updateTimereportsForUser ctx conn)
  where
    selectUsersQuery :: Postgres.Query
    selectUsersQuery = fromString $ unwords $
        [ "SELECT uid FROM planmillproxy.timereports GROUP BY uid"
        ]

-- | Update timereports.
updateAllTimereports
    :: Ctx -> IO ()
updateAllTimereports ctx = runLIO ctx $ \conn -> do
    $(logInfo) $ "Updating timereports for users"

    -- Select uids with oldest updated time reports
    uids <- Postgres.fromOnly <$$> handleSqlError [] (Postgres.query_ conn selectUsersQuery)
    $(logInfo) $ "Updating timereports for users: " <>
        T.intercalate ", " (textShow . getIdent <$> uids)

    for_ uids (updateTimereportsForUser ctx conn)
  where
    getIdent (PM.Ident a) = a

    selectUsersQuery :: Postgres.Query
    selectUsersQuery = fromString $ unwords $
        [ "SELECT u.uid FROM "
        , "(SELECT uid, MIN(updated) as updated FROM planmillproxy.timereports GROUP BY uid) AS u"
        , "ORDER BY u.updated ASC LIMIT 30"
        , ";"
        ]

updateTimereportsForUser :: Ctx -> Postgres.Connection -> PM.UserId -> LIO ()
updateTimereportsForUser ctx conn uid = do
    let interval = $(mkDay "2015-01-01") ... $(mkDay "2017-12-31")
    let q = QueryTimereports (Just interval) uid
    --
    -- Fetch timereports from planmill
    tr <- fetchFromPlanMill (ctxCache ctx) (ctxPlanmillCfg ctx) q

    -- Check what timereports we have stored, remove ones not in planmill anymore
    let planmillTrids = Set.fromList (tr ^.. traverse . PM.identifier)
    postgresTrids <- toTrids <$> handleSqlError []
        (Postgres.query conn selectQuery $ Postgres.Only uid)

    let notInPlanmill = Set.difference postgresTrids planmillTrids
    when (not $ Set.null notInPlanmill) $ do
        let notInPlanmillCount = Set.size notInPlanmill
        $(logInfo) $
            "Found " <> textShow notInPlanmillCount <>
            " timereports not in planmill anymore"
        i <- handleSqlError 0 $ Postgres.execute conn deleteQuery
            (Postgres.Only $ Postgres.In $ Set.toList notInPlanmill)
        when (fromIntegral i /= notInPlanmillCount) $
            $(logWarn) $
                "Deleted " <> textShow i <>
                " out of " <> textShow notInPlanmillCount <> " timereports"

    -- Insert timereports
    _ <- handleSqlError 0 $ insertTimereports conn tr

    -- Done
    pure ()
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

{-
updateRecentTimereports :: Ctx -> IO ()
updateRecentTimereports ctx = runLIO ctx $ \conn -> do
    users <- fetchFromPlanMill (ctxCache ctx) (ctxPlanmillCfg ctx) usersQuery
    let uids = users ^.. traverse . PM.identifier

    -- For each user...
    for_ uids $ \uid -> do
        [(mi, ma)] <- liftIO $ Postgres.query conn selectQuery (Postgres.Only uid)
        $(logInfo) $ "Last updated timereports at " <> textShow (mi :: UTCTime)

        let q = timereportsModifiedQuery uid mi ma
        timereports <- fetchFromPlanMill (ctxCache ctx) (ctxPlanmillCfg ctx) q
        _ <- handleSqlError 0 $ insertTimereports conn timereports

        pure ()
  where
    selectQuery = fromString $ unwords
        [ "SELECT coalesce(max(updated), current_timestamp - ('2 months' :: interval)) - ('1 hour' :: interval), current_timestamp"
        , "FROM planmillproxy.timereports"
        , "WHERE uid = ?"
        , ";"
        ]
-}

-- Helper function to insert timereports
insertTimereports
    :: Foldable f
    => Postgres.Connection
    -> f PM.Timereport
    -> IO Int64
insertTimereports conn trs =
    Postgres.executeMany conn insertQuery $ transformForInsert <$> toList trs
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
-- Capacities
-------------------------------------------------------------------------------

selectCapacities
    :: Ctx -> Postgres.Connection
    -> PM.UserId -> PM.Interval Day -> LIO (Either Text PM.UserCapacities)
selectCapacities ctx conn uid interval = do
    res <- handleSqlError [] $ Postgres.query conn selectQuery (uid, inf interval, sup interval)
    if (length res /= intervalLength)
        then do
            $(logInfo) $
                "Less entries for " <> textShow interval <>
                " capacity interval for " <> textShow uid <>
                ": " <> textShow (length res) <> "/" <> textShow intervalLength
            x <- fetchFromPlanMill (ctxCache ctx) (ctxPlanmillCfg ctx) q
            i <- handleSqlError 0 $ Postgres.executeMany conn insertQuery (transformForInsert x)
            when (fromIntegral i /= length x) $
                $(logWarn) $ "Inserted less capacities than we got from planmill"
            return (Right x)
        else do
            x <- liftIO $ tryDeep $ return $ V.fromList $ map selectTransform res
            return $ first (\y -> show y ^. packed) x
  where
    -- Interval is inclusive on both ends
    intervalLength = fromEnum (sup interval) - fromEnum (inf interval) + 1

    transformForInsert
        :: PM.UserCapacities
        -> [(PM.UserId, Day, Postgres.Binary BSL.ByteString)]
    transformForInsert = fmap f . toList
      where
        f uc = (uid, PM.userCapacityDate uc, Postgres.Binary $ taggedEncode uc)

    insertQuery :: Postgres.Query
    insertQuery = fromString $ unwords $
        [ "INSERT INTO planmillproxy.capacity as c (uid, day, data)"
        , "VALUES (?, ?, ?)"
        , "ON CONFLICT (uid, day) DO UPDATE"
        , "SET  data = EXCLUDED.data, updated = current_timestamp"
        , "WHERE c.uid = EXCLUDED.uid AND c.day = EXCLUDED.day"
        ]

    -- If we gonna planmill we ask some time into a future in advance
    q :: Query PM.UserCapacities
    q = QueryCapacities interval uid

    selectTransform
        :: Postgres.Only (Postgres.Binary BSL.ByteString)
        -> PM.UserCapacity
    selectTransform (Postgres.Only (Postgres.Binary bs)) = taggedDecode bs

    selectQuery :: Postgres.Query
    selectQuery = fromString $ unwords $
        [ "SELECT (data) FROM planmillproxy.capacity"
        , "WHERE uid = ? AND day >= ? AND day <= ?"
        , ";"
        ]

updateCapacities :: Ctx -> IO ()
updateCapacities ctx = runLIO ctx $ \conn -> do
    old <- liftIO $ Postgres.query_ conn selectQuery
    for_ old $ \(uid, mi, ma) ->
        void $ selectCapacities ctx conn uid (mi ... ma)
  where
    -- select intervals which are 6...12 hours old data
    -- clamp the max date to be in current-day + (1 month ... 6 month) interval
    selectQuery :: Postgres.Query
    selectQuery = fromString $ unwords $
        [ "SELECT uid, MIN(day) as min, LEAST(GREATEST(current_date + '1 month' :: interval, MAX(day)), current_date + '6 months' :: interval) :: date as max"
        , "FROM planmillproxy.capacity"
        , "WHERE current_timestamp - updated > (" ++ capacityAge ++ " :: interval) * (1 + variance)"
        , "GROUP BY uid"
        , ";"
        ]

-------------------------------------------------------------------------------
-- Utiltities
-------------------------------------------------------------------------------

-- | Run query on real planmill backend.
fetchFromPlanMill :: DynMapCache -> Cfg -> Query a -> LIO a
fetchFromPlanMill cache cfg q = case typeableDict of
    Dict -> liftIO
        -- TODO: add cache cleanup
        $ genCachedIO RequestNew cache (10 * 60) q
        $ runH cfg $ planmillQuery q
  where
    typeableDict = queryDict (Proxy :: Proxy Typeable) (Sub Dict) q

handleSqlError :: a -> IO a -> LIO a
handleSqlError x action = handle (omitSqlError x) $ liftIO action

omitSqlError :: a -> Postgres.SqlError -> LIO a
omitSqlError a err = do
    $(logError) $ textShow err
    return a

runLoggingT' :: Ctx -> LoggingT IO a -> IO a
runLoggingT' ctx l =
    runStderrLoggingT $ filterLogger p l
  where
    p _ level = level >= ctxLogLevel ctx
