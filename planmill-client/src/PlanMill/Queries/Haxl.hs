{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
-- | A @haxl@ datasource based on 'Query'
--
-- The instance for 'DataSource' is explicit. As we reuse 'Query' as
-- 'DataSource', we pass actual 'fetch' implementation in a 'State'
module PlanMill.Queries.Haxl (
    initDataSourceSimpleIO,
    initDataSourceBatch,
    maxBatchSize,
    PlanmillBatchError (..),
    ) where

import PlanMill.Internal.Prelude

import Haxl.Core

import PlanMill.Types.Cfg   (Cfg)
import PlanMill.Types.Query

-- For initDataSourceSimpleIO
import Control.Monad.CryptoRandom.Extra
       (MonadInitHashDRBG (..), evalCRandTThrow)
import Control.Monad.Http               (runHttpT)
import Control.Monad.Reader             (runReaderT)
import Data.Constraint                  (Dict (..))
import PlanMill.Eval                    (evalPlanMill)
import Numeric.Interval.NonEmpty        (clamp)

-- For initDataSourceBatch
import           Control.Concurrent.Async (async, waitCatch)
import qualified Data.Aeson               as Aeson
import qualified Data.Binary.Tagged       as Binary
import           Data.GADT.Compare        (GEq (..))
import           Data.Type.Equality       ((:~:) (..))
import qualified Network.HTTP.Client      as HTTP
import qualified Network.HTTP.Client.TLS  as HTTP

instance Haxl.Core.Show1 Query where show1 = show

instance StateKey Query where
    data State Query = QueryFunction ([BlockedFetch Query] -> PerformFetch)

instance DataSourceName Query where
    dataSourceName _ = "Planmill.Query"

instance DataSource u Query where
    fetch (QueryFunction f) _flags _userEnv = f

-- | This is a simple query function.
--
-- It's smart enough to reuse http/random-gen for blocked fetches
--
-- /TODO/ take 'HTTP.Manager' as a param
initDataSourceSimpleIO :: Logger -> Cfg -> State Query
initDataSourceSimpleIO lgr cfg = QueryFunction $ \blockedFetches -> SyncFetch $ do
    prng <- mkHashDRBG
    manager <- HTTP.newManager HTTP.tlsManagerSettings
        -- 5 min timeout
        { HTTP.managerResponseTimeout = HTTP.responseTimeoutMicro $ 300 * 1000000
        }

    perform manager prng $ for_ blockedFetches $ \(BlockedFetch q v) ->
        case queryDict (Proxy :: Proxy FromJSON) q of
            Dict -> do
                res <- evalPlanMill $ queryToRequest q
                liftIO $ putSuccess v res
  where
    perform mgr prng
        = flip runHttpT mgr
        . runLogT "planmill-simple" lgr
        . flip runReaderT cfg
        . flip evalCRandTThrow prng

-- | This is batched query function.
--
-- See @planmill-proxy@ for server implementation.
--
-- At this moment uses @application/json@ for requests, and
-- @application/binary-tagged@ for responses.
--
-- See @servant-binary-tagged@.
initDataSourceBatch
    :: Logger
    -> HTTP.Manager  -- ^ Manager
    -> HTTP.Request   -- ^ Base request
    -> State Query
initDataSourceBatch lgr mgr req = QueryFunction queryFunction
  where
    req' = req
        { HTTP.requestHeaders
            = ("Content-Type", "application/json")
            : ("Accept", "application/binary-tagged")
            : HTTP.requestHeaders req
        , HTTP.method
            = "POST"
        }

    queryFunction :: [BlockedFetch Query] -> PerformFetch
    queryFunction blockedFetches = AsyncFetch $ \inner -> do
        -- We execute queries in batches
        let batchSize = clamp (32 ... maxBatchSize) $ 1 + length blockedFetches `div` 4
        let blockedFetchesChunks = chunksOf batchSize blockedFetches
        -- TODO: write own logging lib, we don't like monad-logger that much
        -- startTime <- currentTime
        --
        asyncs <- for blockedFetchesChunks
            $ \bf -> fmap (bf,) . async
            $ runLogT "planmill-haxl" lgr $ do
                logTrace "requesting" (Aeson.toJSON $ map extractQuery bf)
                liftIO $ do
                    res <- HTTP.httpLbs (mkRequest bf) mgr
                    -- Use for debugging:
                    -- print (() <$ res)
                    -- print (BSL.take 1000 $ HTTP.responseBody res)
                    -- print (last $ BSL.toChunks $ HTTP.responseBody res)
                    -- print (BSL.length $ HTTP.responseBody res)
                    let x = Binary.taggedDecode (HTTP.responseBody res) :: [Either Text SomeResponse]

                    -- return blocked fetches as well.
                    evaluate $!! x

        -- Allow inner block to perform
        inner

        -- wait under list and pair
        results <- (traverse . traverse) waitCatch asyncs

        -- endTime <- currentTime
        -- print (startTime, endTime)

        -- Put results
        for_ results $ \res -> case res of
            (bf, Left exc) -> for_ bf $ \(BlockedFetch _ v) -> do
                putFailure' v exc
            (bf, Right res') ->
                putResults bf res'

      where
        mkRequest bf = req'
            { HTTP.requestBody
                = HTTP.RequestBodyLBS $ Aeson.encode
                $ extractQuery <$> bf
            }

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

-- | Maximum haxl request batch sie
--
-- For now it's 128.
maxBatchSize :: Int
maxBatchSize = 128

putFailure' :: Exception e => ResultVar a -> e -> IO ()
putFailure' v = putFailure v . SomeException

-- | Planmill batch error
--
-- See 'initDataSourceBatch'
newtype PlanmillBatchError = PlanmillBatchError Text
    deriving (Show, Typeable)

instance Exception PlanmillBatchError
