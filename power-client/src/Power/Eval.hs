{-# LANGUAGE GADTs #-}
module Power.Eval (
    evalIO,
    ) where

import Futurice.Prelude
import Prelude ()
import Servant.Client
import Servant.Client.Generic

import Power.API
import Power.Request

routes :: PowerRoutes (AsClientT ClientM)
routes = genericClient

evalIO :: BaseUrl -> Manager -> Req a -> IO (Either ServantError a)
evalIO burl mgr req = case req of
    ReqPeople -> runClientM (routePeople routes) env
  where
    env = mkClientEnv mgr burl
