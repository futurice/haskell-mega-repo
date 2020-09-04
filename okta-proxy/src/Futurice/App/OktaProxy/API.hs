{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.OktaProxy.API where

import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant.API
import Servant.API.Generic

data Record route = Record
    { testUrl :: route :- "test" :> Get '[JSON] Bool
    } deriving Generic

type OktaProxyAPI = ToServantApi Record

oktaProxyApi :: Proxy OktaProxyAPI
oktaProxyApi = genericApi (Proxy :: Proxy Record)
