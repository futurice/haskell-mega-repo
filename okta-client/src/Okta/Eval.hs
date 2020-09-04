{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Okta.Eval where

import Data.Aeson       (eitherDecode, encode, object, (.=))
import Futurice.Prelude
import Prelude ()

import Okta.Request
import Okta.Types

import qualified Data.Text                 as T
import qualified Network.HTTP.Client       as HTTP
import qualified Network.HTTP.Types.Status as HTTP

getAfterLink :: HTTP.Response a -> Maybe Text
getAfterLink response =
    let headers = HTTP.responseHeaders response
        links = map (decodeUtf8Lenient . snd) $ filter (\(name, _) -> name == "Link") headers
        checkIfNextLink t = case T.splitOn ";" t of
          [link," rel=\"next\""] -> Just link
          _ -> Nothing
    in T.drop 1 . T.dropEnd 1 <$> listToMaybe (catMaybes $ map checkIfNextLink links)

evalOktaReq :: (MonadThrow m, MonadIO m, MonadLog m, Monad m, MonadReader env m, HasHttpManager env, HasOktaCfg env) => Req a -> m a
evalOktaReq r = case r of
    ReqGetAllUsers        -> pagedReq "/api/v1/users"
    ReqGetAllGroups       -> singleReq "/api/v1/groups"
    ReqGetGroupUsers (OktaGroupId gid)  -> singleReq $ "/api/v1/groups/" <> T.unpack gid <> "/users"
    ReqGetAllApps         -> pagedReq "/api/v1/apps"
    ReqGetAppUsers (OktaAppId aid)    -> pagedReq $ "/api/v1/apps/" <> T.unpack aid <> "/users"
    ReqCreateUser newUser -> postReq "/api/v1/users?activate=false" $ encode $ newUser
    ReqUpdateUser (OktaId uid) userData -> postReq ("/api/v1/users/" <> T.unpack uid) $ encode $ object [ "profile" .= userData]
    ReqAddUserToGroup (OktaGroupId gid) (OktaId uid) -> putReq $ "/api/v1/groups/" <> T.unpack gid <> "/users/" <> T.unpack uid
    ReqRemoveUserFromGroup (OktaGroupId gid) (OktaId uid) -> deleteReq $ "/api/v1/groups/" <> T.unpack gid <> "/users/" <> T.unpack uid
    ReqGetAppLinks (OktaId uid) -> pagedReq $ "/api/v1/users/" <> T.unpack uid <> "/appLinks"
  where
     go _ _ responses Nothing = pure responses
     go mgr token responses (Just url) = do
         request <- HTTP.parseUrlThrow url
         let req = request { HTTP.requestHeaders =
                             ("Authorization",  encodeUtf8 $ "SSWS " <> token)
                             : ("Accept", "application/json")
                             : ("Content-Type", "application/json")
                             : HTTP.requestHeaders request
                           }
         response <- liftIO $ HTTP.httpLbs req mgr
         let res = eitherDecode $ HTTP.responseBody response
         case res of
           Left err -> do
               throwM $ OktaError $ "Error while decoding response: " <> err
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
       request <- HTTP.parseUrlThrow $ T.unpack baseUrl <> endpoint
       let req = request { HTTP.requestHeaders =
                       ("Authorization",  encodeUtf8 $ "SSWS " <> token)
                       : ("Accept", "application/json")
                       : ("Content-Type", "application/json")
                       : HTTP.requestHeaders request}
       response <- liftIO $ HTTP.httpLbs req mgr
       let res = eitherDecode $ HTTP.responseBody response
       case res of
         Left err -> do
             throwM $ OktaError $ "Error while decoding response: " <> err
         Right rs -> do
             pure rs
     postReq endpoint body = do
       mgr <- view httpManager
       (OktaCfg token baseUrl) <- view oktaCfg
       request <- HTTP.parseUrlThrow $ T.unpack baseUrl <> endpoint
       let req = request { HTTP.requestHeaders =
                           ("Authorization",  encodeUtf8 $ "SSWS " <> token)
                           : ("Accept", "application/json")
                           : ("Content-Type", "application/json")
                           : HTTP.requestHeaders request
                         , HTTP.method = "POST"
                         , HTTP.requestBody = HTTP.RequestBodyLBS body }
       response <- liftIO $ HTTP.httpLbs req mgr
       let res = eitherDecode $ HTTP.responseBody response
       case res of
         Left err -> do
             throwM $ OktaError $ "Error while decoding response: " <> err
         Right rs -> do
             pure rs
     putReq endpoint = do
       mgr <- view httpManager
       (OktaCfg token baseUrl) <- view oktaCfg
       request <- HTTP.parseUrlThrow $ T.unpack baseUrl <> endpoint
       let req = request { HTTP.requestHeaders =
                           ("Authorization",  encodeUtf8 $ "SSWS " <> token)
                           : ("Accept", "application/json")
                           : ("Content-Type", "application/json")
                           : HTTP.requestHeaders request
                         , HTTP.method = "PUT" }
       response <- liftIO $ HTTP.httpLbs req mgr
       if HTTP.responseStatus response >= HTTP.status200 && HTTP.responseStatus response < HTTP.status300 then
           pure ()
       else
           throwM $ OktaError $ "Error adding user to group. Got status: " <> show (HTTP.responseStatus response)
     deleteReq endpoint = do
       mgr <- view httpManager
       (OktaCfg token baseUrl) <- view oktaCfg
       request <- HTTP.parseUrlThrow $ T.unpack baseUrl <> endpoint
       let req = request { HTTP.requestHeaders =
                           ("Authorization",  encodeUtf8 $ "SSWS " <> token)
                           : ("Accept", "application/json")
                           : ("Content-Type", "application/json")
                           : HTTP.requestHeaders request
                         , HTTP.method = "DELETE" }
       response <- liftIO $ HTTP.httpLbs req mgr
       if HTTP.responseStatus response >= HTTP.status200 && HTTP.responseStatus response < HTTP.status300 then
           pure ()
       else
           throwM $ OktaError $ "Error adding user to group. Got status: " <> show (HTTP.responseStatus response)

evalOktaReqIO :: OktaCfg -> Manager -> Logger -> Req a -> IO a
evalOktaReqIO cfg mgr lgr req = runLogT "okta-request" lgr $ flip runReaderT (Cfg cfg mgr) $ evalOktaReq req
