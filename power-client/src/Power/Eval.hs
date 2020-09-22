{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Power.Eval (
    evalIO,
    evalIOReq,
    ) where

import Control.Monad.Free
import Futurice.Prelude
import Network.HTTP.Types                 (statusCode)
import Prelude ()
import Servant.Client
import Servant.Client.Free
import Servant.Client.Generic
import Servant.Client.Internal.HttpClient
       (catchConnectionError, clientResponseToResponse, mkFailureResponse)

import qualified Network.HTTP.Client as HTTP

import Power.API
import Power.Request

routes :: PowerRoutes (AsClientT ClientM)
routes = genericClient

evalIO :: BaseUrl -> Manager -> Req a -> IO (Either ClientError a)
evalIO burl mgr req = case req of
    ReqPeople -> runClientM (routePeople routes) env
    ReqAllocation -> runClientM (routeAllocation routes) env
    ReqCustomer -> runClientM (routeCustomer routes) env
    ReqProject -> runClientM (routeProject routes) env
    ReqProjectMapping -> runClientM (routeProjectMapping routes) env
  where
    env = mkClientEnv mgr burl

freeRoutes :: PowerRoutes (AsClientT (Free ClientF))
freeRoutes = genericClient

-- | We essentially implement own servant-client library.
evalIOReq :: HTTP.Request -> Manager -> Req a -> IO (Either ClientError a)
evalIOReq baseReq mgr req = runExceptT $ case req of
    ReqPeople     -> foldFree act (routePeople freeRoutes)
    ReqAllocation -> foldFree act (routeAllocation freeRoutes)
    ReqCustomer   -> foldFree act (routeCustomer freeRoutes)
    ReqProject    -> foldFree act (routeProject freeRoutes)
    ReqProjectMapping -> foldFree act (routeProjectMapping freeRoutes)
  where
    act :: ClientF x -> ExceptT ClientError IO x
    act (Throw err)             = throwError err
    act (RunRequest sReq sRes)  = do
        let httpReq = amendRequest (defaultMakeClientRequest burl sReq)
        httpRes <- liftIO $ catchConnectionError $ HTTP.httpLbs httpReq mgr
        case httpRes of
            Left err -> throwError err
            Right response' -> do
                let status = HTTP.responseStatus response
                    status_code = statusCode status
                    response = response' { HTTP.responseHeaders = mapResHeaders (HTTP.responseHeaders response') }
                    ourResponse = clientResponseToResponse id response
                unless (status_code >= 200 && status_code < 300) $
                    throwError $ mkFailureResponse burl sReq ourResponse
                return (sRes ourResponse)

    burl = BaseUrl
        { baseUrlScheme = if HTTP.port baseReq == 443 then Https else Http
        , baseUrlHost   = decodeUtf8Lenient (HTTP.host baseReq) ^. unpacked
        , baseUrlPort   = HTTP.port baseReq
        , baseUrlPath   = decodeUtf8Lenient (HTTP.path baseReq) ^. unpacked
        }

    amendRequest :: HTTP.Request -> HTTP.Request
    amendRequest r = r
        { HTTP.requestHeaders = HTTP.requestHeaders r ++ HTTP.requestHeaders baseReq
        , HTTP.responseTimeout = HTTP.responseTimeout baseReq
        }

    mapResHeaders = map f where
        f (h@"Content-Type", _) = (h, "application/json;charset=utf-8")
        f h = h
