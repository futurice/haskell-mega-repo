{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}
module Futurice.App.GitHubProxy.Logic (
    -- * Endpoint
    haxlEndpoint,
    -- * Generic cache
    updateCache,
    cleanupCache,
    ) where

import Control.Exception.Base         (fromException)
import Data.Aeson.Types               (FromJSON, parseEither, parseJSON)
import Data.Binary.Get                (Get, runGetOrFail)
import Data.Binary.Tagged
       (Structured, binaryGetMD5, structureHash, structuredDecode,
       structuredEncode)
import Data.Constraint
import Futurice.App.GitHubProxy.H     (runH)
import Futurice.App.GitHubProxy.Types (Ctx (..))
import Futurice.GitHub
       (Auth, Error (..), RW (..), ReqTag, Request, SomeRequest (..),
       SomeResponse (..))
import Futurice.Integrations.Classes  (MonadGitHub (..))
import Futurice.Metrics.RateMeter     (mark)
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant               (Cache, CachePolicy (..), genCachedIO)
import Futurice.TypeTag
import Network.HTTP.Client
       (HttpException (..), HttpExceptionContent (..), path, responseStatus)
import Network.HTTP.Types.Status      (notFound404)
import Prelude ()

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.HashMap.Strict        as HM
import qualified Database.PostgreSQL.Simple as Postgres

-------------------------------------------------------------------------------
-- Intervals
-------------------------------------------------------------------------------

genericAge :: String
genericAge = "'6 hours'"

-------------------------------------------------------------------------------
-- Type synonyms
-------------------------------------------------------------------------------

-- | /TODO/ Store 'SomeResponse' in the database?
type CacheLookup = HashMap SomeRequest BSL.ByteString

lookupCache :: [(SomeRequest, Postgres.Binary BSL.ByteString)] -> CacheLookup
lookupCache ps = HM.fromList (Postgres.fromBinary <$$> ps)

type LIO = LogT IO

runLIO :: Ctx -> LIO a -> IO a
runLIO = runLogT'

-------------------------------------------------------------------------------
-- Logic
-------------------------------------------------------------------------------

-- | The haxl endpoint. We take list of 'Query', and return list of results
haxlEndpoint :: Ctx -> [SomeRequest] -> IO [Either Text SomeResponse]
haxlEndpoint ctx qs = runLIO ctx $ do
    -- Optimistically update view counts
    _ <- safePoolExecute ctx viewQuery postgresQs

    -- Hit the cache for non-primitive queries
    cacheResult <- liftIO $ lookupCache <$> poolQuery ctx selectQuery postgresQs

    -- Info about cache success
    logInfo_ $ "Found "
        <> textShow (HM.size cacheResult) <> " / "
        <> textShow (length qs) <> " (found/all) query results in postgres"

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
    -- sort to prevent deadlocks
    postgresQs  = Postgres.Only . Postgres.In $ sort qs

    -- Fetch provides context for fetch', i.e. this is boilerplate :(
    fetch
        :: CacheLookup -> SomeRequest
        -> LIO (Either Text SomeResponse)
    fetch cacheResult (MkSomeRequest tag req) =
        case (binaryDict, structDict, nfdataDict) of
            (Dict, Dict, Dict) -> fetch' cacheResult tag req
      where
        binaryDict = typeTagDict (Proxy :: Proxy Binary) tag
        structDict = typeTagDict (Proxy :: Proxy Structured) tag
        nfdataDict = typeTagDict (Proxy :: Proxy NFData) tag

    fetch'
        :: forall a. (NFData a, Binary a, Structured a, FromJSON a)
        => CacheLookup  -> ReqTag a -> Request 'RA a
        -> LIO (Either Text SomeResponse)
    fetch' cacheResult tag req = case HM.lookup sreq cacheResult of
        Just bs -> do
            -- we only check tags, the rest of the response is decoded lazily
            -- Hopefully when the end result is constructed.
            if checkTagged (Proxy :: Proxy a) bs
                then pure $ Right $ MkSomeResponse tag $ structuredDecode bs
                else do
                    logAttention_ $ "Borked cache content for " <> textShow sreq
                    _ <- safePoolExecute ctx deleteQuery (Postgres.Only sreq)
                    return $ Left $ "structure tags don't match"
        Nothing -> MkSomeResponse tag <$$> fetch'' tag req
      where
        sreq = MkSomeRequest tag req

    -- Fetch and store
    fetch''
        :: (NFData a, Binary a, Structured a, FromJSON a)
        => ReqTag a -> Request 'RA a -> LIO (Either Text a)
    fetch'' tag req = do
        res <- liftIO $ tryDeep $ runLogT' ctx $ do
            x <- fetchFromGitHub (ctxLogger ctx) (ctxCache ctx) (ctxGitHubAuth ctx) tag req
            storeInPostgres ctx tag req x
            pure $! x
        -- liftIO $ print (res ^? _Left, q)
        return $ first (\x -> ("non-primitive query failure " <> show sreq <> " " <> show x) ^. packed) res
      where
        sreq = MkSomeRequest tag req

    -- Used to update viewed counters
    viewQuery :: Postgres.Query
    viewQuery = "UPDATE githubproxy.cache SET viewed = viewed + 1 WHERE query in ?;"

    -- Used to delete invalid items (cannot decode)
    deleteQuery :: Postgres.Query
    deleteQuery = "DELETE FROM githubproxy.cache WHERE query = ?;"

    -- Select multiple items
    selectQuery :: Postgres.Query
    selectQuery = fromString $ unwords $
        [ "SELECT query, data FROM githubproxy.cache"
        , "WHERE query IN ?"
        , ";"
        ]

-- | Update cache, we look what's viewed the most and update these entries.
-- This means that we never delete items from cache unless there is a error
updateCache :: Ctx -> IO ()
updateCache ctx = runLIO ctx $ do
    qs <- safePoolQuery_ ctx selectQuery
    logInfo_ $ "Updating " <> textShow (length qs) <> " cache items"
    for_ qs $ \(k :: Text, val) -> case parseEither parseJSON val of
        Right (MkSomeRequest tag req) -> do
            r <- fetch tag req
            case r of
                Right () -> pure ()
                Left exc -> do
                    liftIO $ mark "Exception"
                    logAttention "Exception" (show exc)
                    case fromException exc of
                      Just (HTTPError (HttpExceptionRequest req' (StatusCodeException res _))) ->
                          -- remove user request as that users has probably changed
                          when (responseStatus res == notFound404 && "/users/" `BS.isPrefixOf` path req') $ do
                            logAttention "Removing query from database" (show k)
                            void $ safePoolExecute ctx deleteQuery (Postgres.Only k)
                      _ -> pure ()
        Left err -> do
            logAttention "Invalid query" (val, err)
            void $ safePoolExecute ctx deleteQuery (Postgres.Only k)
  where
    fetch :: (FromJSON a) => ReqTag a -> Request 'RA a -> LIO (Either SomeException ())
    fetch tag req  =
        case (binaryDict, structDict, nfdataDict) of
            (Dict, Dict, Dict) -> fetch' tag req
      where
        binaryDict = typeTagDict (Proxy :: Proxy Binary) tag
        structDict = typeTagDict (Proxy :: Proxy Structured) tag
        nfdataDict = typeTagDict (Proxy :: Proxy NFData) tag

    fetch'
      :: (Binary a, Structured a, FromJSON a)
      => ReqTag a -> Request 'RA a -> LIO (Either SomeException ())
    fetch' tag req = liftIO $ tryDeep $ runLogT' ctx $ do
        x <- fetchFromGitHub (ctxLogger ctx) (ctxCache ctx) (ctxGitHubAuth ctx) tag req
        storeInPostgres ctx tag req x

    -- Fetch queries which are old enough, and viewed at least once
    selectQuery :: Postgres.Query
    selectQuery = fromString $ unwords $
        [ "SELECT query :: text, query FROM githubproxy.cache"
        , "WHERE current_timestamp - updated > (" ++ genericAge ++ " :: interval) * (1 + variance) AND viewed > 0"
        , "ORDER BY viewed"
        , "LIMIT 1000"
        , ";"
        ]

    -- Used to delete invalid items (cannot decode)
    deleteQuery :: Postgres.Query
    deleteQuery = "DELETE FROM githubproxy.cache WHERE query = ?;"

-- | Cleanup cache
cleanupCache :: Ctx -> IO ()
cleanupCache ctx = runLIO ctx $ do
    i <- safePoolExecute_ ctx cleanupQuery
    logInfo_ $  "cleaned up " <> textShow i <> " cache items"
  where
    cleanupQuery :: Postgres.Query
    cleanupQuery = fromString $ unwords $
        [ "DELETE FROM githubproxy.cache"
        , "WHERE current_timestamp - updated > '24 hours' AND viewed <= 0"
        , ";"
        ]

storeInPostgres
    :: (Binary a, Structured a, FromJSON a, HasPostgresPool ctx)
    => ctx -> ReqTag a -> Request 'RA a -> a -> LIO ()
storeInPostgres ctx tag req x = do
    -- -- logInfo_ $ "Storing in postgres" <> textShow q
    i <- safePoolExecute ctx postgresQuery (MkSomeRequest tag req, Postgres.Binary $ structuredEncode x)
    when (i == 0) $
        logAttention_ $ "Storing in postgres failed: " <> textShow (MkSomeRequest tag req)
  where
    postgresQuery = fromString $ unwords $
        [ "INSERT INTO githubproxy.cache as c (query, data)"
        , "VALUES (?, ?)"
        , "ON CONFLICT (query) DO UPDATE"
        , "SET data = EXCLUDED.data, viewed = 0, updated = now(), variance = random()"
        , "WHERE c.query = EXCLUDED.query"
        , ";"
        ]

-------------------------------------------------------------------------------
-- Utiltities
-------------------------------------------------------------------------------

-- | Run query on real planmill backend.
fetchFromGitHub :: (FromJSON a) => Logger -> Cache -> Auth -> ReqTag a -> Request 'RA a -> LIO a
fetchFromGitHub logger cache auth tag req = case (typeableDict, nfdataDict, eqDict) of
    (Dict, Dict, Dict) -> liftIO
        -- TODO: add cache cleanup
        $ genCachedIO RequestNew logger cache (10 * 60) req
        $ runH logger auth $ githubReq req
  where
    typeableDict = typeTagDict (Proxy :: Proxy Typeable) tag
    nfdataDict = typeTagDict (Proxy :: Proxy NFData) tag
    eqDict = typeTagDict (Proxy :: Proxy Eq) tag

runLogT' :: Ctx -> LogT IO a -> IO a
runLogT' ctx = runLogT "github-proxy" (ctxLogger ctx)

-------------------------------------------------------------------------------
-- binary-tagged additions
-------------------------------------------------------------------------------

-- | Check whether the tag at the beginning of the 'LazyByteString' is correct.
checkTagged
    :: forall a. (Structured a)
    => Proxy a -> LazyByteString -> Bool
checkTagged _ lbs = either (const False) (view _3) $ runGetOrFail decoder lbs
  where
    decoder :: Get Bool
    decoder = do
        hash' <- binaryGetMD5
        pure $ hash' == hash''

    proxyA = Proxy :: Proxy a
    hash'' = structureHash proxyA
