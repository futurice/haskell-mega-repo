{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- | Not really Cached, but PreRendered
module Servant.Cached (
    Cached,
    mkCached,
    getCached,
    -- * Unsafe
    unsafeMkCached,
    ) where

import Data.Swagger
import Futurice.Prelude
import Prelude ()
import Servant.API

import qualified Data.ByteString.Lazy as LBS

newtype Cached ct a = Cached LBS.ByteString
  deriving (Typeable)

getCached :: Cached ct a -> LBS.ByteString
getCached = coerce

instance NFData (Cached st a) where
    rnf (Cached lbs) = rnf lbs

instance ToSchema a => ToSchema (Cached ct a) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy a)

mkCached :: forall ct a. MimeRender ct a => a -> Cached ct a
mkCached = Cached . mimeRender (Proxy :: Proxy ct)

-- | Creatae from "I know it's correct" data.
unsafeMkCached :: LBS.ByteString -> Cached ct a
unsafeMkCached = Cached

instance (ct ~ ct', MimeRender ct a) => MimeRender ct' (Cached ct a) where
    mimeRender _ = getCached

instance (ct ~ ct', Accept ct) => MimeUnrender ct' (Cached ct a) where
    mimeUnrender _ = Right . Cached
