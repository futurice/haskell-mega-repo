{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.OktaProxy where

import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant
import Servant.Server.Generic

import Futurice.App.OktaProxy.API
import Futurice.App.OktaProxy.Config
import Futurice.App.OktaProxy.Ctx

apiServer :: Ctx -> Server OktaProxyAPI
apiServer ctx = genericServer $ Record
    { testUrl = testUrlImpl ctx
    }

makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO (Ctx, [Job])
makeCtx cfg lgr mgr cache mq = do
    let ctx = Ctx cfg lgr mgr cache

    return (ctx, [])

testUrlImpl :: Ctx -> Handler Bool
testUrlImpl _ = pure True

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverService          .~ OktaProxyService
    & serverDescription      .~ "Okta proxy server"
    & serverColour           .~ (Proxy :: Proxy ('FutuAccent 'AF4 'AC3))
    & serverApp oktaProxyApi .~ apiServer
    & serverEnvPfx           .~ "OKTAPROXY"
