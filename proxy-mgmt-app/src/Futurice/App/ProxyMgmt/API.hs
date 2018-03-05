{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.ProxyMgmt.API where

import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant

type ProxyMgmtAPI =
    SSOUser :> Get '[HTML] (HtmlPage "index")
    :<|> SSOUser :> "regenerate-own-token" :> Post '[JSON] Text
    :<|> SSOUser :> "admin" :> Get '[HTML] (HtmlPage "admin")

proxyMgmtApi :: Proxy ProxyMgmtAPI
proxyMgmtApi = Proxy
