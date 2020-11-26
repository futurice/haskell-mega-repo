{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Futurice.App.OktaProxy.API where

import Futurice.Prelude
import Prelude ()
import Servant.API
import Servant.API.Generic

import Futurice.App.OktaProxy.Types

import qualified FUM.Types.Login as FUM
import qualified Okta            as O
import qualified Personio        as P

data Record route = Record
    { getGroupMembers     :: route :- "group" :> "members" :> ReqBody '[JSON] O.GroupName :> Post '[JSON] [FUM.Login]
    , getUserApplications :: route :- "applications" :> QueryParam' '[Required] "employeeId" P.EmployeeId :> Get '[JSON] (Set AppResponse)
    } deriving Generic

type OktaProxyAPI = ToServantApi Record

oktaProxyApi :: Proxy OktaProxyAPI
oktaProxyApi = genericApi (Proxy :: Proxy Record)
