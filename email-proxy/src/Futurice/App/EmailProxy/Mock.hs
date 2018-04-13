{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Futurice.App.EmailProxy.Mock (defaultMain) where

import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant

import Futurice.App.EmailProxy.API
import Futurice.App.EmailProxy.Types

type Config = ()
type Ctx = Logger

ctxLogger :: Ctx -> Logger
ctxLogger = id

sendEmail :: (MonadIO m, MonadLog m) => Ctx -> Req -> m NoContent
sendEmail _ req = do
    logInfo "Sending mail" req
    pure NoContent

server :: Ctx -> Server EmailProxyAPI
server ctx = pure "This is email proxy mock. See /swagger-ui/"
    :<|> (nt . sendEmail ctx)
  where
    nt :: forall x. LogT Handler x -> Handler x
    nt = runLogT "emailproxy" (ctxLogger ctx)

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService           .~ EmailProxyService
    & serverDescription       .~ "Send Emails"
    & serverColour            .~ (Proxy :: Proxy ('FutuAccent 'AF6 'AC1))
    & serverApp emailProxyApi .~ server
    & serverEnvPfx            .~ "EMAILPROXY"
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> IO (Ctx, [Job])
    makeCtx _cfg logger _mgr _cache = return (logger, [])
