{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Power.API (
    PowerRoutes (..),
    PowerAPI,
    powerApi,
    ) where

import Futurice.Prelude
import Prelude ()
import Servant.API
import Servant.API.Generic

import Power.PyJSON
import Power.Types

data PowerRoutes route = PowerRoutes
    { routePeople         :: route :- "person" :> "" :> Get '[PYJSON] [Person]
    , routeAllocation     :: route :- "allocation" :> "" :> QueryParam "start_date" Day :> QueryParam "end_date" Day :> Get '[PYJSON] [Allocation]
    , routeCustomer       :: route :- "customer" :> "" :> Get '[PYJSON] [Customer]
    , routeProject        :: route :- "project" :> "" :> Get '[PYJSON] [Project]
    , routeProjectMapping :: route :- "powerprojecttoplanmillproject" :> "" :> Get '[PYJSON] [ProjectMapping]
    }
  deriving stock (Generic)

type PowerAPI  = ToServantApi PowerRoutes

powerApi :: Proxy PowerAPI
powerApi = genericApi (Proxy :: Proxy PowerRoutes)
