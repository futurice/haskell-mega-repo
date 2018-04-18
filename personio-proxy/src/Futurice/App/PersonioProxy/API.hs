{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.PersonioProxy.API where

import Futurice.Prelude
import Prelude ()

import Servant

import qualified Personio

type PersonioProxyAPI =
    Get '[JSON] Text
    :<|> "personio-request" :> ReqBody '[JSON] Personio.SomePersonioReq :> Post '[JSON] Personio.SomePersonioRes
    :<|> "employees" :> Get '[JSON] [Personio.Employee]

personioProxyApi :: Proxy PersonioProxyAPI
personioProxyApi = Proxy
