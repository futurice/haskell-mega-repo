module Neleus.Encode where

import Data.Bits      ((.|.))
import Data.Semigroup

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Base16.Lazy as LBS16
import qualified Data.ByteString.Builder     as B
import qualified Data.ByteString.Lazy        as LBS

import Neleus.Types

-- | A wrapper around 'Builder' which tracks the length.
data B = B !Word B.Builder

bLen :: B -> Word
bLen (B n _) = n

instance Semigroup B where
    B l b <> B l' b' = B (l + l') (b <> b')

instance Monoid B where
    mempty = B 0 mempty
    mappend = (<>)

base16encode :: ASN1 -> LBS.ByteString
base16encode = LBS16.encode . encode

encode :: ASN1 -> LBS.ByteString
encode x = case asn1B x of
    B _ b -> B.toLazyByteString b

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

asn1B :: ASN1 -> B
asn1B EOC =
    identifierB UniversalC Primitive 0x00 <> finiteB 0
asn1B (Int i) =
    identifierB UniversalC Primitive 0x02 <> finiteB (bLen b) <> b
  where
    b = integerB i
asn1B (OctetString bs) =
    identifierB UniversalC Primitive 0x04 <> finiteB l <> B l (B.byteString bs)
  where
    l = fromIntegral (BS.length bs)
asn1B (Enum i) =
    identifierB UniversalC Primitive 0x0A <> finiteB (bLen b) <> b
  where
    b = integerB i
asn1B (Sequence xs) =
    identifierB UniversalC Constructed 0x10 <> finiteB (bLen b) <> b
  where
    b = foldMap asn1B xs
asn1B (Application tag x) =
    nonUniversalB ApplicationC tag x
asn1B x = error (show x)

nonUniversalB :: Class -> Tag -> BS -> B
nonUniversalB cls tag (BS bs) = identifierB cls Primitive tag
    <> finiteB l
    <> B l (B.byteString bs)
  where
    l = fromIntegral (BS.length bs)
nonUniversalB cls tag (Val xs) = identifierB cls Constructed tag
    <> finiteB (bLen b)
    <> b
  where
    b = foldMap asn1B xs

-------------------------------------------------------------------------------
-- Integer
-------------------------------------------------------------------------------

integerB :: Integer -> B
integerB i
    | -128 <= i && i <= 127 = B 1 (B.int8 (fromInteger i))
    | otherwise = error "big integer"

-------------------------------------------------------------------------------
-- Identifier
-------------------------------------------------------------------------------

identifierB :: Class -> PC -> Tag -> B
identifierB cls pc tag =
    B 1 (B.word8 (cls' .|. pc' .|. tag'))
  where
    cls' = case cls of
        UniversalC   -> 0x00
        ApplicationC -> 0x40
        ContextC     -> 0x80
        PrivateC     -> 0xc0
    pc' = case pc of
        Primitive   -> 0x00
        Constructed -> 0x20
    tag' | tag < 31 = fromIntegral tag
         | otherwise = error ">= tags not supported"

-------------------------------------------------------------------------------
-- Length
-------------------------------------------------------------------------------

finiteB :: Word -> B
finiteB n
    | n < 0x80  = B 1 (B.word8 (fromIntegral n))
    | otherwise = error "lengthB"

indefiniteB :: B
indefiniteB = B 1 (B.word8 0x80)

lengthB :: Length -> B
lengthB = maybe indefiniteB finiteB

-------------------------------------------------------------------------------
-- test value
-------------------------------------------------------------------------------

searchResDone :: Integer -> ASN1
searchResDone i = Sequence
    [ Int i
    , Application 5 $ Val $
        [ Enum 0 -- success
        -- matchedDN LDAPDN
        , OctetString mempty
        -- errorMessage LDAPString
        , OctetString mempty
        -- referral [3] Referral OPTIONAL
        ]
    ]
