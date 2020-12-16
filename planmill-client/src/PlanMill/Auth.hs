{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- Authentication tools
module PlanMill.Auth (
    Auth(..),
    getAuth,
    getNonce,
    ) where

import PlanMill.Internal.Prelude

import Crypto.Hash.SHA256 (hmac)
import Data.Bits          (shiftL, (.|.))

import qualified Data.ByteString as BS
import qualified Data.Vector     as V

-- Local imports
import PlanMill.Classes
import PlanMill.Types

-- | Authentication information
data Auth = Auth
    { authUser      :: UserId
    , authNonce     :: Nonce
    , authTimestamp :: UTCTime
    , authSignature :: ByteString
    }
    deriving (Eq, Show)

signature :: UserId -> ApiKey -> UTCTime -> Nonce -> ByteString
signature (Ident uid) (ApiKey k) timestamp (Nonce nonce) = digest
  where
    digest = hmac k message

    message :: ByteString
    message = bsShow uid <> nonce <> bsShow (utcTimeToInteger timestamp)


auth :: UserId -> ApiKey -> UTCTime -> Nonce -> Auth
auth u k t n = Auth u n t (signature u k t n)

-- | Create new authentication data for new request.
getAuth :: ( MonadCRandom e m, ContainsCryptoGenError e
           , MonadReader env m, HasPlanMillCfg env
           , MonadTime m
           )
        => m Auth
getAuth = auth
    <$> view planmillCfgUserId
    <*> view planmillCfgApiKey
    <*> currentTime
    <*> getNonce

-- | Generate new fresh nonce.
--
-- Uniqueness isn't checked.
getNonce :: (MonadCRandom e m, ContainsCryptoGenError e) => m Nonce
getNonce = Nonce . fromString . mkNonce <$> getBytes (4 * 8)
  where
    mkNonce :: ByteString -> String
    mkNonce = map mapChar . toWord32s . BS.unpack

    toWord32s :: [Word8] -> [Word32]
    toWord32s (a:b:c:d:ws) = w : toWord32s ws
      where
        w = fromIntegral a `shiftL` 24 .|.
            fromIntegral b `shiftL` 16 .|.
            fromIntegral c `shiftL` 8  .|.
            fromIntegral d `shiftL` 0
    toWord32s _ = []

    mapChar :: Word32 -> Char
    mapChar w = validNonceChars V.! truncate n
      where
        n = (fromIntegral w :: Double)
            * (fromIntegral validNonceCharsLength)
            / (fromIntegral (maxBound :: Word32) + 1)

validNonceChars :: V.Vector Char
validNonceChars =
    V.fromList $ ['A'..'Z'] <> ['a'..'Z'] <> ['0'..'9']

validNonceCharsLength :: Int
validNonceCharsLength = V.length validNonceChars
