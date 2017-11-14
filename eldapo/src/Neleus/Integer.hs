-- | __8.3__ Encoding of an integer value.
--
-- Integers are encoded in two's complement binary.
module Neleus.Integer where

import Data.Int           (Int8)
import Data.List          (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word          (Word8)

encodeInteger :: Integer -> NonEmpty Word8
encodeInteger = error "implement me"

{-

integerB :: Integer -> B
integerB i
    | -128 <= i && i <= 127 = B 1 (B.int8 (fromInteger i))
    | otherwise = error "big integer"

-}

-- | This doesn't check for encoding minimality.
-- i.e. that the bits of the first byte and bit 8 of the second byte
-- aren't all zeros or all ones.
decodeInteger :: NonEmpty Word8 -> Integer
decodeInteger (w :| ws) = foldl' (\x y -> x * 256 + fromIntegral y) w' ws
  where w' = fromIntegral (fromIntegral w :: Int8)
