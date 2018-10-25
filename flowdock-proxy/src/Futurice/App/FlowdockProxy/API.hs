{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.FlowdockProxy.API where

import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant
import Servant.API.Generic

import qualified Chat.Flowdock.REST as FD

data Record route = Record
    { recIndex :: route
        :- QueryParam "needle" Text
        :> QueryParam "nick" Text
        :> QueryParam "flow" FD.FlowId
        :> Get '[HTML] (HtmlPage "index-page")
    , recUsers :: route
        :- "users"
        :> SSOUser
        :> Get '[HTML] (HtmlPage "users-page")
    }
  deriving Generic

type FlowdockProxyAPI = ToServantApi Record

flowdockProxyApi :: Proxy FlowdockProxyAPI
flowdockProxyApi = genericApi (Proxy :: Proxy Record)
