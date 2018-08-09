{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Futurice.App.Proxy.Endpoint (
    -- * Definition
    ProxiedEndpoint,
    -- * Server
    proxyServer,
    ProxyServer,
    HasHttpManager (..),
    HasClientBaseurl (..),
    -- * Endpoints
    proxyEndpoints',
    )where

import Data.Kind         (Type)
import Futurice.Prelude
import Futurice.Services
import GHC.Generics
import Prelude ()
import Servant
import Servant.Client

import qualified Data.Set as Set

-------------------------------------------------------------------------------
-- Proxy
-------------------------------------------------------------------------------

-- | Definition of a proxied endpoint.
--
-- * @service@: service to connect to
--
-- * @private@: service endpoint
--
-- * @public@: public endpoint
--
data ProxiedEndpoint (service :: Service) (private :: Type) (public :: Type)

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

class HasClientBaseurl a (service :: Service) where
    clientBaseurl :: Functor f => Proxy service -> LensLike' f a BaseUrl

-------------------------------------------------------------------------------
-- Proxy Server
-------------------------------------------------------------------------------

type ProxyServer routes = ProxyServerRep (Rep routes)

proxyServer
    :: forall ctx routes. (Generic routes, HasProxy ctx (Rep routes))
    => ctx -> Proxy routes -> Server (ProxyServer routes)
proxyServer ctx _ = proxyServerRep ctx (Proxy :: Proxy (Rep routes))

class HasProxy ctx (rep :: Type -> Type) where
    type ProxyServerRep rep :: Type
    proxyServerRep :: ctx -> Proxy rep -> Server (ProxyServerRep rep)

instance HasProxy ctx f => HasProxy ctx (M1 c i f) where
    type ProxyServerRep (M1 c i f) = ProxyServerRep f
    proxyServerRep ctx _ = proxyServerRep ctx (Proxy :: Proxy f)

instance (HasProxy ctx f, HasProxy ctx g) => HasProxy ctx (f :*: g) where
    type ProxyServerRep (f :*: g) = ProxyServerRep f :<|> ProxyServerRep g
    proxyServerRep ctx _ =
        proxyServerRep ctx (Proxy :: Proxy f) :<|> proxyServerRep ctx (Proxy :: Proxy g)

instance HasProxyK ctx c => HasProxy ctx (K1 r c) where
    type ProxyServerRep (K1 r c) = ProxyServerRepK c
    proxyServerRep ctx _ = proxyServerRepK ctx (Proxy :: Proxy c)

class HasProxyK ctx c where
    type ProxyServerRepK c :: Type
    proxyServerRepK :: ctx -> Proxy c -> Server (ProxyServerRepK c)

instance
    ( HasClient ClientM private
    , Convertible (Client ClientM private) (ServerT public Handler)
    , HasHttpManager ctx, HasClientBaseurl ctx service
    ) => HasProxyK ctx (ProxiedEndpoint service private public)
  where
    type ProxyServerRepK (ProxiedEndpoint service private public) = public
    proxyServerRepK ctx _ = convert cenv (client proxyPrivate)
      where
        cenv = mkClientEnv
            (ctx ^. httpManager)
            (ctx ^. clientBaseurl proxyService)

        proxyPrivate = Proxy :: Proxy private
        proxyService = Proxy :: Proxy service

-------------------------------------------------------------------------------
-- Endpoints
-------------------------------------------------------------------------------

-- | Available endpoints.
proxyEndpoints'
    :: forall routes. (Generic routes, HasEndpoints (Rep routes))
    => Proxy routes -> Set Text
proxyEndpoints' _ = proxyEndpointsRep (Proxy :: Proxy (Rep routes))

class HasEndpoints (rep :: Type -> Type) where
    proxyEndpointsRep :: Proxy rep -> Set Text

instance HasEndpoints f => HasEndpoints (M1 c i f) where
    proxyEndpointsRep _ = proxyEndpointsRep (Proxy :: Proxy f)

instance (HasEndpoints f, HasEndpoints g) => HasEndpoints (f :*: g) where
    proxyEndpointsRep _ =
        proxyEndpointsRep (Proxy :: Proxy f) <>
        proxyEndpointsRep (Proxy :: Proxy g)

instance HasEndpointsK c => HasEndpoints (K1 r c) where
    proxyEndpointsRep _ = proxyEndpointsRepK (Proxy :: Proxy c)

class HasEndpointsK c where
    proxyEndpointsRepK :: Proxy c -> Set Text

instance (HasPrefix public) => HasEndpointsK (ProxiedEndpoint service private public) where
    proxyEndpointsRepK _
        = Set.singleton
        $ foldMap ("/" <>)
        $ endpointPrefix (Proxy :: Proxy public)

-- | Helper class
class HasPrefix api where
    endpointPrefix :: Proxy api -> [Text]

instance (KnownSymbol sym, HasPrefix api) => HasPrefix (sym :> api) where
    endpointPrefix _ = textVal (Proxy :: Proxy sym) : endpointPrefix (Proxy :: Proxy api)

instance HasPrefix (Verb m s ct a) where
    endpointPrefix _ = []

instance HasPrefix api => HasPrefix (Summary d :> api) where
    endpointPrefix _ = endpointPrefix (Proxy :: Proxy api)

instance HasPrefix api => HasPrefix (Capture' mods n a :> api) where
    endpointPrefix _ = endpointPrefix (Proxy :: Proxy api)

instance HasPrefix api => HasPrefix (QueryParam' mods n a :> api) where
    endpointPrefix _ = endpointPrefix (Proxy :: Proxy api)

instance HasPrefix api => HasPrefix (QueryFlag n :> api) where
    endpointPrefix _ = endpointPrefix (Proxy :: Proxy api)

instance HasPrefix api => HasPrefix (ReqBody' mods ct a :> api) where
    endpointPrefix _ = endpointPrefix (Proxy :: Proxy api)
