-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.Auth (
    Nonce(..),
    ApiKey(..),
    ) where

import Prelude ()
import PlanMill.Internal.Prelude
import Futurice.EnvConfig        (FromEnvVar (..))

import qualified Data.ByteString    as BS


-- | Unique 4-8 characters long string composed of upper and lower case letters
-- and numbers. Each nonce can only be used once so it must be regenerated for
-- each request
newtype Nonce = Nonce BS.ByteString
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance Hashable Nonce
instance NFData Nonce

-- | Also known as signature.
newtype ApiKey = ApiKey BS.ByteString
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance Hashable ApiKey
instance NFData ApiKey
instance IsString ApiKey where
    fromString = ApiKey . fromString

instance FromJSON ApiKey where
    parseJSON = withText "Planmill apikey" $ pure . ApiKey . encodeUtf8

instance FromEnvVar ApiKey where
    fromEnvVar = fmap ApiKey . fromEnvVar
