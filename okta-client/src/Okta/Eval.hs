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

getAfterLink :: HTTP.Response a -> Maybe Text
getAfterLink response =
    let headers = HTTP.responseHeaders response
        links = map (decodeUtf8Lenient . snd) $ filter (\(name, _) -> name == "Link") headers
        checkIfNextLink t = case T.splitOn ";" t of
          [link," rel=\"next\""] -> Just link
          _ -> Nothing
    in T.drop 1 . T.dropEnd 1 <$> listToMaybe (catMaybes $ map checkIfNextLink links)

evalOktaReq :: (MonadThrow m, MonadIO m, Monad m, MonadReader env m, HasHttpManager env, HasOktaCfg env) => Req a -> m a
evalOktaReq r = case r of
    ReqGetAllUsers  -> pagedReq "/api/v1/users"
    ReqGetAllGroups -> singleReq "/api/v1/groups"
    ReqGetGroupUsers gid -> singleReq $ "/api/v1/groups/" <> T.unpack gid <> "/users"
  where
     go _ _ responses Nothing = pure responses
     go mgr token responses (Just url) = do
         request <- HTTP.parseRequest url
         let req = request { HTTP.requestHeaders =
                             ("Authorization",  encodeUtf8 $ "SSWS " <> token)
                             : ("Accept", "application/json")
                             : ("Content-Type", "application/json")
                             : HTTP.requestHeaders request
                           }
         response <- liftIO $ HTTP.httpLbs req mgr
         let res = eitherDecode $ HTTP.responseBody response
         case res of --TODO: throw exception
           Left err -> do
               void $ liftIO $ print err
               pure []
           Right rs -> do
               let afterLink = getAfterLink response
               go mgr token (responses <> rs) (T.unpack <$> afterLink)
     pagedReq endpoint = do
         mgr <- view httpManager
         (OktaCfg token baseUrl) <- view oktaCfg
         go mgr token [] (Just $ T.unpack baseUrl <> endpoint)
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
         Right rs -> do
             pure rs

evalOktaReqIO :: OktaCfg -> Manager -> Req a -> IO a
evalOktaReqIO cfg mgr req = flip runReaderT (Cfg cfg mgr) $ evalOktaReq req
