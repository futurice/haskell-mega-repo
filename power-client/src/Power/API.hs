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

import Power.Types
import Power.PyJSON

data PowerRoutes route = PowerRoutes
    { routePeople :: route :- "person" :> "" :> Get '[PYJSON] [Person]
    -- TODO: add more routes
    }
  deriving stock (Generic)

type PowerAPI  = ToServantApi PowerRoutes

powerApi :: Proxy PowerAPI
powerApi = genericApi (Proxy :: Proxy PowerRoutes)
