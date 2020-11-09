{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Slack.Eval where

import Data.Aeson
import Futurice.Prelude
import Prelude ()

import Slack.Types

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
                                 ,"text"    .= message]
            , HTTP.method = "POST"
            , HTTP.requestHeaders = ("Authorization", encodeUtf8 $ "Bearer " <> token)
                                    : ("Content-Type", "application/json")
                                    : HTTP.requestHeaders req
            }
    response <- liftIO $ HTTP.httpLbs req' mgr
    pure ()

evalSlackReqIO :: SlackToken -> Manager -> Req a -> IO a
evalSlackReqIO token mgr req = flip runReaderT (Cfg token mgr) $ evalSlackReq req
