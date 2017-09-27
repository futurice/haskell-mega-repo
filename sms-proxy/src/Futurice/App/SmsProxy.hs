{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.SmsProxy(defaultMain) where

import Prelude ()
import Futurice.Prelude
import Futurice.Servant
import Servant

import Futurice.App.SmsProxy.API
import Futurice.App.SmsProxy.Config
import Futurice.App.SmsProxy.Types
import Futurice.App.SmsProxy.Ctx
import Futurice.App.SmsProxy.Logic

server :: Ctx -> Server SmsProxyAPI
server ctx = pure "This is sms proxy" 
    :<|> (\a b -> nt $ sendLegacySms' ctx a b)
    :<|> (nt . (sendSms ctx))
  where
    nt :: forall x. LogT Handler x -> Handler x
    nt = runLogT "smsproxy" (ctxLogger ctx)

sendLegacySms'
    :: (MonadIO m, MonadLog m, MonadThrow m, MonadError ServantErr m)
    => Ctx
    -> Maybe Text
    -> Maybe Text
    -> m Text
sendLegacySms' ctx to msg = do
    to' <- maybe (throwError $ err400 { errBody = "'to' is required" }) pure to
    msg' <- maybe (throwError $ err400 { errBody = "'msg' is required" }) pure msg
    sendLegacySms ctx (Req to' msg')

defaultMain :: IO ()
defaultMain = futuriceServerMain makeCtx $ emptyServerConfig
    & serverName            .~ "SMS Proxy"
    & serverDescription     .~ "Send sms"
    & serverColour          .~ (Proxy :: Proxy ('FutuAccent 'AF5 'AC2))
    & serverApp smsProxyApi .~ server
    & serverEnvPfx          .~ "SMSPROXY"
  where
    makeCtx :: Config -> Logger -> Cache -> IO (Ctx, [Job])
    makeCtx cfg lgr _cache = do
        mgr <- newManager tlsManagerSettings
        return (Ctx lgr cfg mgr, [])
