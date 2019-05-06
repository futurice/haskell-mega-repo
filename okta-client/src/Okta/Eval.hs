{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Okta.Eval where

import Data.Aeson       (eitherDecode)
import Futurice.Prelude
import Prelude ()

import Okta.Request
import Okta.Types

import qualified Data.Text           as T
import qualified Network.HTTP.Client as HTTP

evalOktaReq :: (MonadThrow m, MonadIO m, Monad m, MonadReader env m, HasHttpManager env, HasOktaCfg env) => Req a -> m a
evalOktaReq r = case r of
    ReqGetAllUsers  -> singleReq "/api/v1/users"
    ReqGetAllGroups -> singleReq "/api/v1/groups"
    ReqGetGroupUsers gid -> singleReq $ "/api/v1/groups/" <> T.unpack gid <> "/users"
  where
     singleReq endpoint = do
       mgr <- view httpManager
       (OktaCfg token baseUrl) <- view oktaCfg
       request <- HTTP.parseRequest $ T.unpack baseUrl <> endpoint
       let req = request { HTTP.requestHeaders =
                       ("Authorization",  encodeUtf8 $ "SSWS " <> token)
                       : ("Accept", "application/json")
                       : ("Content-Type", "application/json")
                       : HTTP.requestHeaders request}
       response <- liftIO $ HTTP.httpLbs req mgr
       let res = eitherDecode $ HTTP.responseBody response
       case res of --TODO: throw exception
         Left err -> do
             void $ liftIO $ print err
             pure []
         Right rs -> pure rs

evalOktaReqIO :: OktaCfg -> Manager -> Req a -> IO a
evalOktaReqIO cfg mgr req = flip runReaderT (Cfg cfg mgr) $ evalOktaReq req
