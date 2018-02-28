{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.ProxyMgmt.API where

import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant

type ProxyMgmtAPI = SSOUser :> ProxyMgmtAPI'

type ProxyMgmtAPI' =
    Get '[HTML] (HtmlPage "index")
    -- :<|> "dashdo" :> SSOUser :> DashdoAPI

proxyMgmtApi :: Proxy ProxyMgmtAPI
proxyMgmtApi = Proxy

proxyMgmtApi' :: Proxy ProxyMgmtAPI'
proxyMgmtApi' = Proxy
