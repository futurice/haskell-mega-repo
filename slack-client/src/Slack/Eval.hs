{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Slack.Eval where

import Data.Aeson       (eitherDecode, encode, object, (.=))
import Futurice.Prelude
import Prelude ()

import Slack.Types

import qualified Data.Aeson.Lens     as L
import qualified Data.Text           as T
import qualified Network.HTTP.Client as HTTP

evalSlackReq :: (MonadIO m, MonadThrow m, HasHttpManager env, HasSlackToken env, MonadReader env m) => Req a -> m a
evalSlackReq (ReqSendMessage (ChannelId cid) message) = do
    let url = "https://slack.com/api/chat.postMessage"
    req <- HTTP.parseRequest url
    mgr <- view httpManager
    (SlackToken token) <- view slackToken
    let req' = req
            { HTTP.requestBody = HTTP.RequestBodyLBS $ encode $ object
                                 ["channel" .= cid
                                 ,"blocks"    .= [(
                                       object ["type" .= (tid "section")
                                              ,"text" .= (object ["type" .= (tid "mrkdwn")
                                                                 , "text" .= message
                                                                 ])])]]
            , HTTP.method = "POST"
            , HTTP.requestHeaders = ("Authorization", encodeUtf8 $ "Bearer " <> token)
                                    : ("Content-Type", "application/json")
                                    : HTTP.requestHeaders req
            }
    response <- liftIO $ HTTP.httpLbs req' mgr
    let res = (eitherDecode $ HTTP.responseBody response :: Either String Value)
    case res of
      Right res' ->
          case res' ^? L.key "ok" . L._Bool of
            Just True -> pure ()
            _ -> throwM $ SlackError $ T.unpack $ fromMaybe "Couldn't decode response" $ res' ^? L.key "error" . L._String
      Left err -> throwM $ SlackError err
  where
   tid :: Text -> Text
   tid = id

evalSlackReqIO :: SlackToken -> Manager -> Req a -> IO a
evalSlackReqIO token mgr req = flip runReaderT (Cfg token mgr) $ evalSlackReq req
