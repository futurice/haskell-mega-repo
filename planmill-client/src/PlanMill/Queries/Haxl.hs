{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-- | A @haxl@ datasource based on 'Query'
--
-- The instance for 'DataSource' is explicit. As we reuse 'Query' as
-- 'DataSource', we pass actual 'fetch' implementation in a 'State'
module PlanMill.Queries.Haxl (
    initDataSourceSimpleIO,
    initDataSourceBatch,
    PlanmillBatchError (..),
    ) where

import PlanMill.Internal.Prelude

import Haxl.Core

import PlanMill.Types.Cfg   (Cfg)
import PlanMill.Types.Query

-- For initDataSourceSimpleIO
import Control.Monad.Logger             (LogLevel, filterLogger)
import Control.Monad.CryptoRandom.Extra
       (MonadInitHashDRBG (..), evalCRandTThrow)
import Control.Monad.Http               (evalHttpT)
import Control.Monad.Reader             (runReaderT)
import Data.Constraint                  (Dict (..))
import PlanMill.Eval                    (evalPlanMill)

-- For initDataSourceBatch
import           Control.Concurrent.Async (async, waitCatch)
import qualified Data.Aeson               as Aeson
import qualified Data.Binary.Tagged       as Binary
import           Data.GADT.Compare        (GEq (..))
import           Data.Type.Equality       ((:~:) (..))
import qualified Network.HTTP.Client      as HTTP

instance Show1 Query where show1 = show

instance StateKey Query where
    data State Query = QueryFunction ([BlockedFetch Query] -> PerformFetch)

instance DataSourceName Query where
    dataSourceName _ = "Planmill.Query"

instance DataSource u Query where
    fetch (QueryFunction f) _flags _userEnv = f

-- | This is a simple query function.
--
-- It's smart enough to reuse http/random-gen for blocked fetches
initDataSourceSimpleIO :: LogLevel -> Cfg -> State Query
initDataSourceSimpleIO loglevel cfg = QueryFunction $ \blockedFetches -> SyncFetch $ do
    g <- mkHashDRBG
    perform g $ for_ blockedFetches $ \(BlockedFetch q v) ->
        case queryDict (Proxy :: Proxy FromJSON) q of
            Dict -> do
                res <- evalPlanMill $ queryToRequest q
                liftIO $ putSuccess v res
  where
    perform g
        = evalHttpT
        . runStderrLoggingT
        . filterLogger logPred
        . flip runReaderT cfg
        . flip evalCRandTThrow g
    logPred _ = (>= loglevel)

-- | This is batched query function.
--
-- See @planmill-proxy@ for server implementation.
--
-- At this moment uses @application/json@ for requests, and
-- @application/binary-tagged@ for responses.
--
-- See @servant-binary-tagged@.
initDataSourceBatch
    :: HTTP.Manager  -- ^ Manager
    -> HTTP.Request  -- ^ Base request
    -> State Query
initDataSourceBatch mgr req = QueryFunction queryFunction
  where
    req' = req
        { HTTP.requestHeaders
            = ("Content-Type", "application/json")
            : ("Accept", "application/binary-tagged")
            : HTTP.requestHeaders req
        , HTTP.method
            = "POST"
        }
    queryFunction blockedFetches = AsyncFetch $ \inner -> do
        a <- async $ do
            x <- Binary.taggedDecode . HTTP.responseBody <$> HTTP.httpLbs req'' mgr
            evaluate $!! x
        inner
        res <- waitCatch a
        case res of
            Left exc -> for_ blockedFetches $ \(BlockedFetch _ v) ->
                putFailure' v exc
            Right res' ->
                putResults blockedFetches res'

      where
        reqBody = Aeson.encode $ extractQuery <$> blockedFetches
        req'' = req' { HTTP.requestBody = HTTP.RequestBodyLBS reqBody }

    extractQuery :: BlockedFetch Query -> SomeQuery
    extractQuery (BlockedFetch q _) = SomeQuery q

    putResults :: [BlockedFetch Query] -> [Either Text SomeResponse] -> IO ()
    -- if no more blocked fetches, we are done
    putResults [] _ =
        return ()
    -- if we have blocked fetch, but no result:
    -- report error
    putResults (BlockedFetch _q v : _)  [] =
        putFailure' v (PlanmillBatchError "Truncated response")
    -- backend might return error as well
    putResults (BlockedFetch _q v : rest) (Left err : bss) = do
        putFailure' v (PlanmillBatchError err)
        putResults rest bss
    -- on success, try decode
    putResults (BlockedFetch  q v : rest) (Right res : bss) = do
        putResult v (coerceResponse q res)
        putResults rest bss

    coerceResponse :: Query a -> SomeResponse -> Either SomeException a
    coerceResponse q (MkSomeResponse q' r) = case geq q q' of
        Just Refl -> pure r
        Nothing   -> throwM (PlanmillBatchError "Unmatching response")

putFailure' :: Exception e => ResultVar a -> e -> IO ()
putFailure' v = putFailure v . SomeException

-- | Planmill batch error
--
-- See 'initDataSourceBatch'
newtype PlanmillBatchError = PlanmillBatchError Text
    deriving (Show, Typeable)

instance Exception PlanmillBatchError
