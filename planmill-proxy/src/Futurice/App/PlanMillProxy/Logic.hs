{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -freduction-depth=0 #-}
#endif
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
    -- * Stats
    statsEndpoint,
    ) where

import Prelude ()
import Futurice.Prelude
import Data.Aeson.Compat     (object, (.=))
import Data.Binary.Tagged
       (HasSemanticVersion, HasStructuralInfo, taggedDecode, taggedEncode)
import Data.Constraint
import Futurice.PostgresPool
import PlanMill.Types.Query
       (Query (..), SomeQuery (..), SomeResponse (..), queryDict)

import qualified Data.ByteString.Lazy       as BSL
import qualified Data.HashMap.Strict        as HM
import qualified Database.PostgreSQL.Simple as Postgres

import Futurice.App.PlanMillProxy.Logic.Capacities
import Futurice.App.PlanMillProxy.Logic.Common
import Futurice.App.PlanMillProxy.Logic.Timereports
import Futurice.App.PlanMillProxy.Types             (Ctx (..), Stats (..))

-------------------------------------------------------------------------------
-- Stats
-------------------------------------------------------------------------------

statsEndpoint :: Ctx -> IO Stats
statsEndpoint ctx = do
    -- let's fail if we get not exactly one result
    [(cAvg, cMin, cMax, cTotal)] <- poolQuery_ ctx $ fromString $ unwords
        [ "SELECT"
        , "extract(epoch from avg(current_timestamp - updated)),"
        , "extract(epoch from min(current_timestamp - updated)),"
        , "extract(epoch from max(current_timestamp - updated)),"
        , "count(*)"
        , "FROM planmillproxy.cache;"
        ]

    [(trAvg, trMin, trMax, trTotal)] <- poolQuery_ ctx $ fromString $ unwords
        [ "SELECT"
        , "extract(epoch from avg(current_timestamp - updated)),"
        , "extract(epoch from min(current_timestamp - updated)),"
        , "extract(epoch from max(current_timestamp - updated)),"
        , "count(*)"
        , "FROM planmillproxy.timereports;"
        ]

    return $ Stats cAvg cMin cMax cTotal trAvg trMin trMax trTotal

-------------------------------------------------------------------------------
-- Type synonyms
-------------------------------------------------------------------------------

-- | /TODO/ Store 'SomeResponse' in the database?
type CacheLookup = HashMap SomeQuery BSL.ByteString

lookupCache :: [(SomeQuery, Postgres.Binary BSL.ByteString)] -> CacheLookup
lookupCache ps = HM.fromList (Postgres.fromBinary <$$> ps)

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

-- | The haxl endpoint. We take list of 'Query', and return list of results
haxlEndpoint :: Ctx -> [SomeQuery] -> IO [Either Text SomeResponse]
haxlEndpoint ctx qs = runLIO ctx $ do
    -- Optimistically update view counts
    _ <- handleSqlError 0 $ poolExecute ctx viewQuery postgresQs

    -- Hit the cache for non-primitive queries
    cacheResult <- liftIO $ lookupCache <$> poolQuery ctx selectQuery postgresQs

    -- Info about cache success
    logInfo_ $ "Found "
        <> textShow (HM.size cacheResult) <> " / "
        <> textShow (length primitiveQs) <> " / "
        <> textShow (length qs) <> " (found/primitive/all) query results in postgres"

    -- go thru each request
    -- primitive requests are handled one by one, that could be optimised.
    res <- traverse (fetch cacheResult) qs

    -- Log the first errorneous response
    case (res ^? folded . _Left) of
        Nothing  -> pure ()
        Just err -> logAttention_ $ "haxl response contains errors : " <> err

    -- return
    pure res
  where
    primitiveQs = filter isPrimitive qs
    postgresQs  = Postgres.Only . Postgres.In $ primitiveQs

    -- We handle timereports and capacities specially
    isPrimitive (SomeQuery (QueryCapacities _ _))  = False
    isPrimitive (SomeQuery (QueryTimereports _ _)) = False
    isPrimitive _                                  = True

    -- Fetch provides context for fetch', i.e. this is boilerplate :(
    fetch
        :: CacheLookup -> SomeQuery
        -> LIO (Either Text SomeResponse)
    fetch _cacheResult (SomeQuery q@(QueryCapacities i u)) =
        Right . MkSomeResponse q <$> selectCapacities ctx u i
    fetch _cacheResult (SomeQuery q@(QueryTimereports i u)) =
        Right . MkSomeResponse q <$> selectTimereports ctx u i
    fetch cacheResult (SomeQuery q) =
        case (binaryDict, semVerDict, structDict, nfdataDict) of
            (Dict, Dict, Dict, Dict) -> fetch' cacheResult q
      where
        binaryDict = queryDict (Proxy :: Proxy Binary) q
        semVerDict = queryDict (Proxy :: Proxy HasSemanticVersion) q
        structDict = queryDict (Proxy :: Proxy HasStructuralInfo) q
        nfdataDict = queryDict (Proxy :: Proxy NFData) q

    fetch'
        :: forall a. (NFData a, Binary a, HasSemanticVersion a, HasStructuralInfo a)
        => CacheLookup  -> Query a
        -> LIO (Either Text SomeResponse)
    fetch' cacheResult q = case HM.lookup (SomeQuery q) cacheResult of
        Just bs -> do
            -- we only check tags, the rest of the response is decoded lazily
            -- Hopefully when the end result is constructed.
            if checkTagged (Proxy :: Proxy a) bs
                then pure $ Right $ MkSomeResponse q $ taggedDecode bs
                else do
                    logAttention_ $ "Borked cache content for " <> textShow q
                    _ <- handleSqlError 0 $
                        poolExecute ctx deleteQuery (Postgres.Only q)
                    return $ Left $ "structure tags don't match"
        Nothing -> MkSomeResponse q <$$> fetch'' q

    -- Fetch and store
    fetch''
        :: (NFData a, Binary a, HasSemanticVersion a, HasStructuralInfo a)
        => Query a -> LIO (Either Text a)
    fetch'' q = do
        res <- liftIO $ tryDeep $ runLogT' ctx $ do
            x <- fetchFromPlanMill ctx q
            storeInPostgres ctx q x
            pure $! x
        -- liftIO $ print (res ^? _Left, q)
        return $ first (\x -> ("non-primitive query failure " <> show q <> " " <> show x) ^. packed) res

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
updateCache ctx = runLIO ctx $ do
    qs <- handleSqlError [] $ poolQuery_ ctx selectQuery
    logInfo_ $ "Updating " <> textShow (length qs) <> " cache items"
    for_ qs $ \(Postgres.Only (SomeQuery q)) -> do
        res <- fetch q
        case res of
            Right () -> pure ()
            Left exc -> do
                logAttention "Update failed" $ object [ "query" .= q, "exc" .= show exc ]
                void $ handleSqlError 0 $ poolExecute ctx deleteQuery (Postgres.Only q)
  where
    fetch :: Query a -> LIO (Either SomeException ())
    fetch q =
        case (binaryDict, semVerDict, structDict, nfdataDict) of
            (Dict, Dict, Dict, Dict) -> fetch' q
      where
        binaryDict = queryDict (Proxy :: Proxy Binary) q
        semVerDict = queryDict (Proxy :: Proxy HasSemanticVersion) q
        structDict = queryDict (Proxy :: Proxy HasStructuralInfo) q
        nfdataDict = queryDict (Proxy :: Proxy NFData) q

    fetch'
        :: (Binary a, HasStructuralInfo a, HasSemanticVersion a)
        => Query a -> LIO (Either SomeException ())
    fetch' q = liftIO $ tryDeep $ runLogT' ctx $ do
        x <- fetchFromPlanMill ctx q
        storeInPostgres ctx q x

    -- Fetch queries which are old enough, and viewed at least once
    selectQuery :: Postgres.Query
    selectQuery = fromString $ unwords $
        [ "SELECT (query) FROM planmillproxy.cache"
        -- , "WHERE current_timestamp - updated > (" ++ genericAge ++ " :: interval) * (1 + variance) AND viewed > 0"
        , "WHERE updated < date_trunc('day', now() at time zone 'Europe/Helsinki') at time zone 'Europe/Helsinki' + '2 hours' :: interval"
        , "  AND viewed >= 1"
        , "ORDER BY updated"
        , "LIMIT 1000"
        , ";"
        ]

    deleteQuery :: Postgres.Query
    deleteQuery = fromString $ unwords $
        [ "DELETE FROM planmillproxy.cache"
        , "WHERE query = ?"
        , ";"
        ]

-- | Cleanup cache
cleanupCache :: Ctx -> IO ()
cleanupCache ctx = runLIO ctx $ do
    i <- handleSqlError 0 $ poolExecute_ ctx cleanupQuery
    logInfo_ $  "cleaned up " <> textShow i <> " cache items"
  where
    cleanupQuery :: Postgres.Query
    cleanupQuery = fromString $ unwords $
        [ "DELETE FROM planmillproxy.cache"
        , "WHERE current_timestamp - updated > '48 hours' AND viewed <= 0"
        , ";"
        ]

storeInPostgres
    :: (Binary a, HasSemanticVersion a, HasStructuralInfo a, HasPostgresPool ctx)
    => ctx -> Query a -> a -> LIO ()
storeInPostgres ctx q x = do
    -- -- logInfo_ $ "Storing in postgres" <> textShow q
    i <- handleSqlError 0 $
        poolExecute ctx postgresQuery (q, Postgres.Binary $ taggedEncode x)
    when (i == 0) $
        logAttention_ $ "Storing in postgres failed: " <> textShow q
  where
    postgresQuery = fromString $ unwords $
        [ "INSERT INTO planmillproxy.cache as c (query, data)"
        , "VALUES (?, ?)"
        , "ON CONFLICT (query) DO UPDATE"
        , "SET data = EXCLUDED.data, viewed = 0, updated = now(), variance = random()"
        , "WHERE c.query = EXCLUDED.query"
        , ";"
        ]
