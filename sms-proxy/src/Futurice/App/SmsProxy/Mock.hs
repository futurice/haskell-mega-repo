{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Futurice.App.SmsProxy.Mock where

import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant

import Futurice.App.SmsProxy.API
import Futurice.App.SmsProxy.Config
import Futurice.App.SmsProxy.Types

type Ctx = Logger

server :: Ctx -> Server SmsProxyAPI
server ctx = pure "This is sms proxy"
    :<|> (\a b -> nt $ (sendLegacySms' a b))
    :<|> (nt . sendSms)
  where
    nt :: forall x. LogT Handler x -> Handler x
    nt = runLogT "smsproxy" ctx

sendLegacySms' :: (MonadLog m) => Maybe Text -> Maybe Text -> m Text
sendLegacySms' to message = do
    _ <- case (to, message) of
           (Nothing, _) -> logInfo_ "No number"
           (_, Nothing) -> logInfo_ "No message"
           (Just to', Just message') -> logInfo "Sending legacy sms" $ Req to' message'
    pure ""

sendSms :: (MonadLog m) => Req -> m Res
sendSms req = do
    logInfo "Sending sms" req
    pure $ Res "" ""

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService         .~ SmsProxyService
    & serverDescription     .~ "Send sms"
    & serverColour          .~ (Proxy :: Proxy ('FutuAccent 'AF6 'AC1))
    & serverApp smsProxyApi .~ server
    & serverEnvPfx          .~ "SMSPROXY"
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
    makeCtx _cfg lgr _mgr _cache _mq = do
        return (lgr, [])
