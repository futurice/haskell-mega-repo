{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
-- | Interface to @github-proxy@.
module Futurice.Integrations.GitHub where

import Control.Concurrent.Async  (async, waitCatch)
import Data.GADT.Compare         (geq)
import Data.Type.Equality        ((:~:) (..))
import Futurice.Generics
import Futurice.Prelude
import Numeric.Interval.NonEmpty (clamp, (...))
import Prelude ()

import qualified Codec.Compression.GZip as GZip
import qualified Data.Aeson             as Aeson
import qualified Data.Binary.Tagged     as Binary
import qualified Futurice.GitHub        as GH
import qualified Haxl.Core              as H
import qualified Network.HTTP.Client    as HTTP

-- | Init Haxl data source.
initDataSource :: Logger -> Manager -> HTTP.Request -> H.State GHR
initDataSource = GHState

-- | Haxl github request.
data GHR a = (FromJSON a) => GHR !(GH.ReqTag a) !(GH.Request 'GH.RA a)

instance Eq (GHR a) where
    GHR t r == GHR t' r' = GH.MkSomeRequest t r == GH.MkSomeRequest t' r'

instance Hashable (GHR a) where
    hashWithSalt salt (GHR t r) = hashWithSalt salt (GH.MkSomeRequest t r)

instance Show (GHR a) where
    showsPrec d (GHR _ r) = showsPrec d r

instance H.ShowP GHR where showp = show

instance H.StateKey GHR where
    data State GHR = GHState !Logger !Manager !HTTP.Request

instance H.DataSourceName GHR where
    dataSourceName _ = "GitHub.Request"

instance H.DataSource u GHR where
    fetch (GHState lgr mgr baseReq) _flags _userEnv = H.AsyncFetch $ \blockedFetches inner -> do
        -- We execute queries in batches
        let batchSize = clamp (32 ... maxBatchSize) $ 1 + length blockedFetches `div` 4
        let blockedFetchesChunks = chunksOf batchSize blockedFetches
        -- TODO: write own logging lib, we don't like monad-logger that much
        -- startTime <- currentTime
        --
        asyncs <- for blockedFetchesChunks
            $ \bf -> fmap (bf,) . async
            $ runLogT "github-haxl" lgr $ do
                logTrace "request" (Aeson.toJSON $ map extractQuery $ take 10 $ bf)
                liftIO $ do
                    res  <- HTTP.httpLbs (mkRequest bf) mgr
                    -- Use for debugging:
                    -- print (() <$ res)
                    -- print (BSL.take 1000 $ HTTP.responseBody res)
                    -- print (last $ BSL.toChunks $ HTTP.responseBody res)
                    -- print (BSL.length $ HTTP.responseBody res)
                    let x = Binary.taggedDecode (GZip.decompress $ HTTP.responseBody res) :: [Either Text GH.SomeResponse]

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
            (bf, Left exc) -> for_ bf $ \(H.BlockedFetch _ v) -> do
                putFailure' v exc
            (bf, Right res') ->
                putResults bf res'

      where
        baseReq' = baseReq
            { HTTP.requestHeaders
                = ("Content-Type", "application/json")
                : ("Accept", "application/gzip-binary-tagged")
                : HTTP.requestHeaders baseReq
            , HTTP.method
                = "POST"
            }

        mkRequest bf = baseReq'
            { HTTP.requestBody
                = HTTP.RequestBodyLBS $ Aeson.encode
                $ extractQuery <$> bf
            }

        extractQuery :: H.BlockedFetch GHR -> GH.SomeRequest
        extractQuery (H.BlockedFetch (GHR t r) _) = GH.MkSomeRequest t r

        putResults :: [H.BlockedFetch GHR] -> [Either Text GH.SomeResponse] -> IO ()
        -- if no more blocked fetches, we are done
        putResults [] _ =
            return ()
        -- if we have blocked fetch, but no result:
        -- report error
        putResults (H.BlockedFetch _q v : _)  [] =
            putFailure' v (GitHubBatchError "Truncated response")
        -- backend might return error as well
        putResults (H.BlockedFetch _q v : rest) (Left err : bss) = do
            putFailure' v (GitHubBatchError err)
            putResults rest bss
        -- on success, try decode
        putResults (H.BlockedFetch (GHR t _) v : rest) (Right res : bss) = do
            H.putResult v (coerceResponse t res)
            putResults rest bss

        coerceResponse :: GH.ReqTag a -> GH.SomeResponse -> Either SomeException a
        coerceResponse t (GH.MkSomeResponse t' r) = case geq t t' of
            Just Refl -> pure r
            Nothing   -> throwM (GitHubBatchError "Unmatching response")

-- | Maximum haxl request batch sie
--
-- For now it's 128.
maxBatchSize :: Int
maxBatchSize = 128

putFailure' :: Exception e => H.ResultVar a -> e -> IO ()
putFailure' v = H.putFailure v . SomeException

-- | GitHub batch error
newtype GitHubBatchError = GitHubBatchError Text
    deriving (Show, Typeable)

instance Exception GitHubBatchError
