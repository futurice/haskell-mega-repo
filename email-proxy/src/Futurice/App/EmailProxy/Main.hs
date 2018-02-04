{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Futurice.App.EmailProxy.Main (defaultMain) where

import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant

import qualified Network.AWS as AWS

import Futurice.App.EmailProxy.API
import Futurice.App.EmailProxy.Config
import Futurice.App.EmailProxy.Ctx
import Futurice.App.EmailProxy.Logic

server :: Ctx -> Server EmailProxyAPI
server ctx = pure "This is email proxy. See /swagger-ui/"
    :<|> (nt . sendEmail ctx)
  where
    nt :: forall x. LogT Handler x -> Handler x
    nt = runLogT "emailproxy" (ctxLogger ctx)

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverName              .~ "Email Proxy"
    & serverDescription       .~ "Send Emails"
    & serverColour            .~ (Proxy :: Proxy ('FutuAccent 'AF5 'AC2))
    & serverApp emailProxyApi .~ server
    & serverEnvPfx            .~ "EMAILPROXY"
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> IO (Ctx, [Job])
    makeCtx cfg logger _mgr _cache = do
        env' <- AWS.newEnv (cfgSesCreds cfg)
        -- SES is only in Ireland in Europe
        let env = env' & AWS.envRegion .~ AWS.Ireland
        return (Ctx logger cfg env, [])
