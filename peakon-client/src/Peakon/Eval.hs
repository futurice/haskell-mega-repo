{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Peakon.Eval where

import Data.Aeson       (Value (..), eitherDecode, encode, object, (.=))
import Futurice.Prelude
import Prelude ()

import Peakon.Request
import Peakon.Types

import qualified Data.Aeson.Lens     as L
import qualified Data.Text           as T
import qualified Network.HTTP.Client as HTTP

getAfterLink :: (L.AsValue a) => HTTP.Response a -> Maybe Text
getAfterLink response = HTTP.responseBody response ^? L.key "links" . L.key "next" . L._String

getAuth :: (MonadThrow m, MonadIO m, MonadLog m, Monad m, MonadReader env m, HasHttpManager env, HasPeakonCfg env) => m Text
getAuth = do
    (PeakonCfg token _) <- view peakonCfg
    response <- postReq "/v1/auth/application" $ encode $ object [ "token" .= token
                                                                 , "persists" .= True]
    case HTTP.responseBody response ^? L.key "data" . L.key "id" . L._String of
        Just jwt -> pure jwt
        Nothing -> throwM $ PeakonError $ "Authentication failure"
  where
    postReq endpoint body = do
       mgr <- view httpManager
       (PeakonCfg _ baseUrl) <- view peakonCfg
       request <- HTTP.parseUrlThrow $ T.unpack baseUrl <> endpoint
       let req = request { HTTP.requestHeaders =
                           ("Accept", "application/json")
                           : ("Content-Type", "application/json")
                           : HTTP.requestHeaders request
                         , HTTP.requestBody = HTTP.RequestBodyLBS body
                         , HTTP.method = "POST"
                         }
       liftIO $ HTTP.httpLbs req mgr

evalPeakonReq :: (MonadThrow m, MonadIO m, MonadLog m, Monad m, MonadReader env m, HasHttpManager env, HasPeakonCfg env) => Req a -> m a
evalPeakonReq req = case req of
    ReqEngagementOverview -> singleReq "/v1/engagement/overview"
    ReqEngagementDrivers  -> singleReq "/v1/engagement/drivers"
    ReqSegments           -> pagedReq "/v1/segments"
  where
    singleReq endpoint = do
        jwt <- getAuth
        mgr <- view httpManager
        (PeakonCfg _ baseUrl) <- view peakonCfg
        request <- HTTP.parseUrlThrow $ T.unpack baseUrl <> endpoint
        let req' = request { HTTP.requestHeaders =
                            ("Authorization", encodeUtf8 $ "Bearer " <> jwt)
                            : ("Accept", "application/json")
                            : ("Content-Type", "application/json")
                            : HTTP.requestHeaders request}
        response <- liftIO $ HTTP.httpLbs req' mgr
        let res = eitherDecode $ HTTP.responseBody response
        case res of
          Left err -> do
              throwM $ PeakonError $ "Error while decoding response: " <> err
          Right rs -> do
              pure rs
    extractData :: Value -> Maybe (Vector Value)
    extractData r = r ^? L.key "data" . L._Array
    go :: (MonadThrow m, MonadIO m, MonadLog m, Monad m, MonadReader env m, HasHttpManager env, HasPeakonCfg env) => (Maybe String) -> Text -> List Value -> m Value
    go Nothing _ responses = pure $ Array $ fold $ catMaybes $ fmap extractData responses
    go (Just endpoint) jwt responses = do
        mgr <- view httpManager
        request <- HTTP.parseUrlThrow endpoint
        let req' = request { HTTP.requestHeaders =
                            ("Authorization", encodeUtf8 $ "Bearer " <> jwt)
                            : ("Accept", "application/json")
                            : ("Content-Type", "application/json")
                            : HTTP.requestHeaders request}
        response <- liftIO $ HTTP.httpLbs req' mgr
        let res = eitherDecode $ HTTP.responseBody response
        case res of
          Left err -> do
              throwM $ PeakonError $ "Error while decoding response: " <> err
          Right rs -> do
              let afterLink = getAfterLink response
              go (T.unpack <$> afterLink) jwt (responses <> [rs])
    pagedReq endpoint = do
         jwt <- getAuth
         (PeakonCfg _ baseUrl) <- view peakonCfg
         go (Just $ T.unpack baseUrl <> endpoint) jwt []

evalPeakonReqIO :: PeakonCfg -> Manager -> Logger -> Req a -> IO a
evalPeakonReqIO cfg mgr lgr req = runLogT "peakon-request" lgr $ flip runReaderT (Cfg cfg mgr) $ evalPeakonReq req
