{-# LANGUAGE OverloadedStrings #-}
module Futurice.Wai.ContentMiddleware (contentMiddleware) where

import Control.Lens        (_last)
import Data.IORef
import Futurice.Prelude
import Network.HTTP.Types  (RequestHeaders, hAccept, statusCode)
import Network.Wai
       (Application, Middleware, ResponseReceived, pathInfo, requestHeaders,
       responseStatus, responseStream, responseToStream)
import Prelude ()
import Servant.API         (Accept, JSON, contentTypes)
import Servant.CSV.Cassava (CSV)
import Servant.HTML.Lucid  (HTML)

import qualified Codec.Compression.Zlib.Internal as Zlib
import qualified Data.ByteString.Builder         as B
import qualified Data.ByteString.Lazy            as LBS
import qualified Data.ByteString            as BS
import qualified Data.Text                       as T
import qualified Network.HTTP.Media              as M

-- | Middleware which matches the path on @.html@, @.csv@, @.json@ etc,
-- and redirects to suffixless endpoint with proper @Accept@ header.
--
-- Therefore we can /force/ the content-type for endpoints.
-- That is sometimes handy.
--
contentMiddleware :: Middleware
contentMiddleware = compressMiddleware . contentTypeMiddleware

-------------------------------------------------------------------------------
-- Content-Type
-------------------------------------------------------------------------------

contentTypeMiddleware :: Middleware
contentTypeMiddleware app req sendRes = app req $ \res -> do
    let def = sendRes res
    if statusCode (responseStatus res) /= 404
    then def
    else case pathInfo req ^? _last of
        Just pathPart
            | Just pathPart' <- ".json" `T.stripSuffix` pathPart ->
                redirect (Proxy :: Proxy JSON) pathPart' def app req sendRes
            | Just pathPart' <- ".csv" `T.stripSuffix` pathPart ->
                redirect (Proxy :: Proxy CSV)  pathPart' def app req sendRes
            | Just pathPart' <- ".html" `T.stripSuffix` pathPart ->
                redirect (Proxy :: Proxy HTML) pathPart' def app req sendRes

            | otherwise -> def
        Nothing         -> def

redirect :: Accept ct => Proxy ct -> Text -> IO ResponseReceived -> Application -> Application
redirect p pathPart' def app req sendRes =
    case M.matchAccept (toList (contentTypes p)) hAcceptValue of
        Nothing  -> def
        Just acc -> do
            let req' = req
                    { pathInfo       = pathInfo req & _last .~ pathPart'
                    , requestHeaders = changeAccept acc $ requestHeaders req
                    }
            app req' sendRes
  where
    hAcceptValue = fromMaybe ctWildcard $ lookup hAccept $ requestHeaders req

changeAccept :: M.MediaType -> RequestHeaders -> RequestHeaders
changeAccept acc = go where
    go []              = [(hAccept, M.renderHeader acc)]
    go ((n, v) : hs)
        | n == hAccept = (n, M.renderHeader acc) : hs
        | otherwise    = (n, v) : go hs

ctWildcard :: ByteString
ctWildcard = "*/*"

-------------------------------------------------------------------------------
-- Compress
-------------------------------------------------------------------------------

compressMiddleware :: Middleware
compressMiddleware app req sendRes = app req $ \res -> do
    let def = sendRes res
    if statusCode (responseStatus res) /= 404
    then def
    else case pathInfo req ^? _last of
        Just pathPart
            | Just pathPart' <- ".gz" `T.stripSuffix` pathPart ->
                compressGzip pathPart' app req sendRes

            | otherwise -> def
        Nothing         -> def

compressGzip :: Text -> Application -> Application
compressGzip pathPart' app req sendRes = do
    -- we don't check whether requester supports gzip; we simply return it (it asks for .gz)
    let req' = req
          { pathInfo = pathInfo req & _last .~ pathPart'
          -- change accept to be anything:
          , requestHeaders = changeAccept "*/*" $ requestHeaders req
          }

    app req' $ \res -> sendRes $ case responseToStream res of
        (status, headers, streamK) -> responseStream status headers $
            \send flush -> streamK $ \ stream -> do
                ref <- newIORef $ Zlib.compressIO Zlib.gzipFormat Zlib.defaultCompressParams

                let send' :: B.Builder -> IO ()
                    send' b = do
                        let bs = LBS.toStrict $ B.toLazyByteString b
                        if BS.null bs
                        then return ()
                        else do

                            -- loop until we get CompressInputRequired
                            let loop (Zlib.CompressInputRequired f) = do
                                    s <- f bs
                                    writeIORef ref s

                                loop (Zlib.CompressOutputAvailable bs' next) = do
                                    send (B.byteString bs')
                                    s <- next
                                    loop s

                                loop s@Zlib.CompressStreamEnd = do
                                    writeIORef ref s
                                    return ()

                            -- let the loop roll
                            readIORef ref >>= loop

                -- process stream
                stream send' flush

                -- finalize loop
                let endLoop (Zlib.CompressInputRequired f) = do
                        s <- f mempty
                        endLoop s

                    endLoop (Zlib.CompressOutputAvailable bs next) = do
                        send (B.byteString bs)
                        s <- next
                        endLoop s

                    endLoop Zlib.CompressStreamEnd = return ()

                readIORef ref >>= endLoop
