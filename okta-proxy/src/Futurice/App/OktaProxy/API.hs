{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.OktaProxy.API where

import Futurice.Prelude
import Prelude ()
import Servant.API
import Servant.API.Generic

import qualified FUM.Types.Login as FUM

data Record route = Record
    { getGroupMembers :: route :- "group" :> "members" :> ReqBody '[JSON] Text :> Post '[JSON] [FUM.Login]
    } deriving Generic

type OktaProxyAPI = ToServantApi Record

oktaProxyApi :: Proxy OktaProxyAPI
oktaProxyApi = genericApi (Proxy :: Proxy Record)
