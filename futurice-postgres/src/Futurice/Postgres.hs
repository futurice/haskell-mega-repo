{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Wrappers of "Data.PostgreSQL.Simple" query and execution
-- functions, to work with @'Pool' 'Database.PostgreSQL.Simple.Connection'@.
--
-- Note: do not use this with transactions; they won't work.
module Futurice.Postgres (
    -- * Class
    HasPostgresPool(..),
    Pool,
    Postgres.Connection,
    -- * Construction
    createPostgresPool,
    -- * Query
    poolQuery,
    poolQuery_,
    -- * Execute
    poolExecute,
    poolExecute_,
    poolExecuteMany,
    -- * Transaction
    poolWithTransaction,
    -- * Safe
    safePoolQuery,
    safePoolQuery_,
    safePoolExecute,
    safePoolExecute_,
    safePoolExecuteMany,
    -- * Re-export types
    Postgres.ConnectInfo,
    Postgres.Query,
    ) where

import Control.Monad.Catch        (Handler (..), catches)
import Data.Pool                  (Pool, createPool, withResource)
import Data.Typeable              (typeRep)
import Futurice.Metrics.RateMeter (mark)
import Futurice.Prelude
import Prelude ()

import qualified Data.Aeson.Compat          as Aeson
import qualified Database.PostgreSQL.Simple as Postgres

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

class HasPostgresPool a where
    postgresPool :: a -> Pool Postgres.Connection

instance conn ~ Postgres.Connection => HasPostgresPool (Pool conn) where
    postgresPool = id

-------------------------------------------------------------------------------
-- Creation
-------------------------------------------------------------------------------

createPostgresPool :: MonadIO m => Postgres.ConnectInfo -> m (Pool Postgres.Connection)
createPostgresPool ci = liftIO $ createPool
    (Postgres.connect ci)
    Postgres.close
    2 60 5
    -- 2 stripes
    -- 1 minute
    -- 5 connections per pool

-------------------------------------------------------------------------------
-- Execute
-------------------------------------------------------------------------------

poolExecute
    :: (Postgres.ToRow q, HasPostgresPool ctx, MonadBaseControl IO m)
    => ctx -> Postgres.Query -> q -> m Int64
poolExecute ctx query row = withResource (postgresPool ctx) $ \conn ->
    liftBase $ Postgres.execute conn query row

poolExecute_
    :: (HasPostgresPool ctx, MonadBaseControl IO m)
    => ctx -> Postgres.Query -> m Int64
poolExecute_ ctx query = withResource (postgresPool ctx) $ \conn ->
    liftBase $ Postgres.execute_ conn query

poolExecuteMany
    :: (Postgres.ToRow q, HasPostgresPool ctx, MonadBaseControl IO m)
    => ctx -> Postgres.Query -> [q] -> m Int64
poolExecuteMany ctx query rows = withResource (postgresPool ctx) $ \conn ->
    liftBase $ Postgres.executeMany conn query rows

-------------------------------------------------------------------------------
-- Query
-------------------------------------------------------------------------------

poolQuery
    :: (Postgres.ToRow q, Postgres.FromRow r, HasPostgresPool ctx, MonadBaseControl IO m)
    => ctx -> Postgres.Query -> q -> m [r]
poolQuery ctx query row = withResource (postgresPool ctx) $ \conn ->
    liftBase $ Postgres.query conn query row

poolQuery_
    :: (Postgres.FromRow r, HasPostgresPool ctx, MonadBaseControl IO m)
    => ctx -> Postgres.Query -> m [r]
poolQuery_ ctx query = withResource (postgresPool ctx) $ \conn ->
    liftBase $ Postgres.query_ conn query

-------------------------------------------------------------------------------
-- Transaction
-------------------------------------------------------------------------------

poolWithTransaction
    :: (HasPostgresPool ctx, MonadBaseControl IO m, StM m a ~ a)
    => ctx -> m a -> m a
poolWithTransaction ctx inner = withResource (postgresPool ctx) $ \conn ->
    liftBaseWith $ \runInBase ->
        Postgres.withTransaction conn (runInBase inner)

-------------------------------------------------------------------------------
-- Safe
-------------------------------------------------------------------------------

safePoolQuery
    :: (Postgres.ToRow q, Postgres.FromRow r, HasPostgresPool ctx, MonadBaseControl IO m, MonadLog m, MonadCatch m)
    => ctx -> Postgres.Query -> q -> m [r]
safePoolQuery ctx query row = handleSqlError query [] $
    poolQuery ctx query row

safePoolQuery_
    :: (Postgres.FromRow r, HasPostgresPool ctx, MonadBaseControl IO m, MonadLog m, MonadCatch m)
    => ctx -> Postgres.Query -> m [r]
safePoolQuery_ ctx query = handleSqlError query [] $
    poolQuery_ ctx query

safePoolExecute
    :: (Postgres.ToRow q, HasPostgresPool ctx, MonadBaseControl IO m, MonadLog m, MonadCatch m)
    => ctx -> Postgres.Query -> q -> m Int64
safePoolExecute ctx query row = handleSqlError query 0 $
    poolExecute ctx query row

safePoolExecute_
    :: (HasPostgresPool ctx, MonadBaseControl IO m, MonadLog m, MonadCatch m)
    => ctx -> Postgres.Query -> m Int64
safePoolExecute_ ctx query = handleSqlError query 0 $
    poolExecute_ ctx query

safePoolExecuteMany
    :: (Postgres.ToRow q, HasPostgresPool ctx, MonadBaseControl IO m, MonadLog m, MonadCatch m)
    => ctx -> Postgres.Query -> [q] -> m Int64
safePoolExecuteMany ctx query rows = handleSqlError query 0 $
    poolExecuteMany ctx query rows

handleSqlError
    :: forall a m. (MonadBaseControl IO m, MonadLog m, MonadCatch m)
    => Postgres.Query -> a -> IO a -> m a
handleSqlError q x action = catches (liftBase action)
    [ Handler (omitError q x :: Postgres.SqlError -> m a)
    , Handler (omitError q x :: Aeson.AesonException -> m a)
    ]

omitError
    :: forall e a m. (Exception e, MonadBase IO m, MonadLog m)
    => Postgres.Query -> a -> e -> m a
omitError q a err = do
    liftBase $ mark "Omitted SQL error"
    logAttention (textShow (typeRep (Proxy :: Proxy e)) <> textShow err) (show q)
    return a
