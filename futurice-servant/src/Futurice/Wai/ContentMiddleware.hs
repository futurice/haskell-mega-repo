{-# LANGUAGE OverloadedStrings #-}
module Futurice.Wai.ContentMiddleware (contentMiddleware) where

import Control.Lens        (_last)
import Futurice.Prelude
import Network.HTTP.Types  (RequestHeaders, hAccept, statusCode)
import Network.Wai
       (Application, Middleware, ResponseReceived, pathInfo, requestHeaders,
       responseStatus)
import Prelude ()
import Servant.API         (Accept, JSON, contentTypes)
import Servant.CSV.Cassava (CSV)
import Servant.HTML.Lucid  (HTML)

import qualified Data.Text          as T
import qualified Network.HTTP.Media as M

-- | Middleware which matches the path on @.html@, @.csv@, @.json@ etc,
-- and redirects to suffixless endpoint with proper @Accept@ header.
--
-- Therefore we can /force/ the content-type for endpoints.
-- That is sometimes handy.
--
-- TODO: support @.gz@ and @.xz@.
--
contentMiddleware :: Middleware
contentMiddleware app req sendRes = app req $ \res -> do
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

redirect :: Accept ct => Proxy ct -> Text -> IO ResponseReceived ->  Application -> Application
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
