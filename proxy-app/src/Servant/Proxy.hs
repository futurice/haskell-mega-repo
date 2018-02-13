{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Servant.Proxy (
    makeProxy,
    ProxyPair,
    ProxyServer,
    -- * Classes
    HasHttpManager (..),
    HasClientBaseurl (..),
    -- * Internal
    Convertible,
    ) where

import Prelude ()
import Futurice.Prelude
import Servant
import Servant.Client

-------------------------------------------------------------------------------
-- Proxy definitions
-------------------------------------------------------------------------------

-- | Definition of a proxy.
--
-- * @public@: public endpoint
--
-- * @service@: service to connect to
--
-- * @private@: service endpoint
--
data ProxyPair public service private

-- | Extract servant api for public part of the proxy.
type family ProxyServer def where
    ProxyServer '[p]      = ProxyServer' p
    ProxyServer (p ': ps) = ProxyServer' p :<|> ProxyServer ps

type family ProxyServer' def where
    ProxyServer' (ProxyPair public _s _p) = public

-------------------------------------------------------------------------------
-- convert ClientM to Handler
-------------------------------------------------------------------------------

-- | Class to convert client functions to server functions
class Convertible client server | client -> server, server -> client where
    convert :: ClientEnv -> client -> server

instance Convertible pub priv => Convertible (a -> pub) (a -> priv) where
    convert env cli x = convert env (cli x)

instance Convertible (ClientM a) (Handler a) where
    convert env cli = mk $ runClientM cli env
      where
        mk action = liftIO action >>= either (throwError . transformError) pure
        transformError err = err504 { errBody = fromString $ show err }

-------------------------------------------------------------------------------
-- Few helper optic classes
-------------------------------------------------------------------------------

class HasHttpManager a where
    httpManager :: Lens' a Manager

class HasClientBaseurl a service where
    clientBaseurl :: forall f. Functor f => Proxy service -> LensLike' f a BaseUrl

-------------------------------------------------------------------------------
-- Putting everything together
-------------------------------------------------------------------------------

-- | Class describing proxable endpoints
--
-- 'ClientEnv' paramater is taken tagged, so we don't mix them.
makeProxy
    :: forall env public service private.
       ( HasClient ClientM private
       , Convertible (Client ClientM private) (ServerT public Handler)
       , HasHttpManager env, HasClientBaseurl env service
       )
    => Proxy (ProxyPair public service private)
    -> env -> Server public
makeProxy _ env = convert env' (client proxyPrivate)
  where
    env' = mkClientEnv
        (env ^. httpManager)
        (env ^. clientBaseurl proxyService)

    proxyPrivate = Proxy :: Proxy private
    proxyService = Proxy :: Proxy service
