{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- | Not really Cached, but PreRendered
module Servant.Cached (
    Cached,
    mkCached,
    ) where

import Futurice.Prelude
import Prelude ()
import Servant.API
import Data.Swagger

import qualified Data.ByteString.Lazy as LBS


newtype Cached ct a = Cached LBS.ByteString
  deriving (Typeable)

instance NFData (Cached st a) where
    rnf (Cached lbs) = rnf lbs

instance ToSchema a => ToSchema (Cached ct a) where
    declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy a)

mkCached :: forall ct a. MimeRender ct a => a -> Cached ct a
mkCached = Cached . mimeRender (Proxy :: Proxy ct)

instance MimeRender ct a => MimeRender ct (Cached ct a) where
    mimeRender _ (Cached lbs) = lbs
