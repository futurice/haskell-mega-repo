{-# LANGUAGE MultiWayIf #-}
-- | Decoding according to Basic Endcoding Rules (BER).
--
-- See <https://www.itu.int/ITU-T/studygroups/com17/languages/X.690-0207.pdf ITU-T Recommendation X.690>.
module Neleus.Decode where

-- TODO:

import Data.Bits ((.&.))
import Data.Int  (Int8)
import Data.List (foldl')

import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString            as BS

import Neleus.Types

import Debug.Trace

asn1P :: A.Parser ASN1
asn1P = do
    -- identifier octets
    hdr@(Identifier cls pc tag) <- identifierParser
    -- length octets
    len <- lengthParser
    -- content octets
    case cls of
        UniversalC -> case traceShow hdr tag of
            -- 8.1.5 End-of-contents octets
            0x00 -> do
                l <- requirePrimitive pc len
                requireBool (l == 0) "End-of-contents content's should be absent"
                return EOC

            -- 8.2 Encoding of a boolean value
            0x01 -> do
                -- 8.2.1 boolean should be primitive
                l <- requirePrimitive pc len
                -- should consist of a single octet
                requireBool (l == 1) "Bool: shall consist of a single octet"
                w <- A.anyWord8
                -- 8.2.2 False encoded as 0, True as everything else
                return $ Bool $ w /= 0

            -- 8.3 Encoding of an integer value
            0x02 -> do
                i <- integerParser pc len
                return $ Int i

            -- 8.7 Encoding of an octetstring value
            0x04 -> do
                -- TODO: constructed encoding is different.
                l <- requirePrimitive pc len
                bs <- A.take l
                return $ OctetString bs

            -- 8.8 Encoding of a null value
            0x05 -> do
                -- 8.8.1 null should be primitive
                l <- requirePrimitive pc len
                -- 8.8.2 should not contain any octets
                requireBool (l == 0) "Null shall not contain any octets"
                return Null

            -- 8.4 Encoding of an enumerated value
            0x0A -> do
                -- encoded as an integer value
                i <- integerParser pc len
                return $ Enum i
            -- 8.9 Encoding of a sequence value
            0x10 -> do
                -- 8.9.1 sequence should be constructed
                requireConstructed pc
                l <- requireFinite len
                bs <- A.take l
                xs <- either fail pure $ A.parseOnly (A.manyTill asn1P A.endOfInput) bs
                pure $ Sequence xs

            _ -> do
                fail $ "Unknown universal tag " ++ show hdr

        -- Non universal types.
        ApplicationC -> do
            xs <- asn1BSP pc len
            pure $ Application tag xs

        ContextC -> do
            xs <- asn1BSP pc len
            pure $ Context tag xs

        PrivateC -> do
            xs <- asn1BSP pc len
            pure $ Private tag xs

-- | non universal types are encoded similarly.
--
-- X.680 says that there is no difference between non-universal tags.
-- It's a matter of choice and style
-- Three classes are there for historical reasons.
--
-- See note in X.680 (07/2002) 8.3
asn1BSP :: PC -> Length -> A.Parser BS
asn1BSP pc len = do
    l <- requireFinite len
    bs <- A.take l
    case pc of
        Primitive -> pure $ BS bs
        Constructed -> either fail (pure . Val) $
            A.parseOnly (A.manyTill asn1P A.endOfInput) bs

-------------------------------------------------------------------------------
-- 8.3 Encoding of an integer value
-------------------------------------------------------------------------------

integerParser :: PC -> Length -> A.Parser Integer
integerParser pc len = do
    -- 8.3.1 integer should be primitive
    l <- requirePrimitive pc len
    -- 8.3.1 should be one or more octets
    requireBool (l >= 1) "Integer: Shall consist >=1 octets"
    -- 8.3.3 Encoded as a complement binary number
    w <- A.anyWord8
    -- TODO: 8.2.2 (check minimal encoding)
    let i = fromIntegral w :: Int8
    bs <- A.take (l - 1)
    return $ foldl' (\x y -> x * 256 + fromIntegral y) (fromIntegral i) (BS.unpack bs)

-------------------------------------------------------------------------------
-- Guards
-------------------------------------------------------------------------------

requireBool :: Bool -> String -> A.Parser ()
requireBool True _    = pure ()
requireBool False err = fail err

requireFinite :: Length -> A.Parser Int
requireFinite = maybe
    (fail "Primitive should be of finite length")
    (pure . fromIntegral)

requirePrimitive :: PC -> Length -> A.Parser Int
requirePrimitive Primitive   (Just n) = pure (fromIntegral n)
requirePrimitive Constructed _        = fail "Primitive encoding expected"
requirePrimitive Primitive   Nothing  = fail "Primitive should be of finite length"

requireConstructed :: PC -> A.Parser ()
requireConstructed Constructed = pure ()
requireConstructed Primitive   = fail "Constructed encoding expected"

-------------------------------------------------------------------------------
-- Identifier
-------------------------------------------------------------------------------

identifierParser :: A.Parser Identifier
identifierParser = do
    -- 8.1.2 Identifier octets
    w <- A.anyWord8
    -- 8.1.2.5: Bit 6: 0 = primitive, 1 = constructed
    let pc =
            if (w .&. 0x20) == 0x00
            then Primitive
            else Constructed
    -- Table 1 - Encoding of class of tag
    let cls = case w .&. 0xc0 of
            0x00 -> UniversalC
            0x40 -> ApplicationC
            0x80 -> ContextC
            _ {- 0xc0 -} -> PrivateC
    let tag' = w .&. 0x1f
    if tag' /= 0x1f
    then pure $ Identifier cls pc $ fromIntegral tag'
    else fail "not implemented: >=31 tags"

-------------------------------------------------------------------------------
-- Length
-------------------------------------------------------------------------------

lengthParser :: A.Parser Length
lengthParser = do
    w <- A.anyWord8
    if | w .&. 0x80 == 0 -> pure $ Just $ fromIntegral w
       | w == 0x80       -> pure Nothing
       | otherwise -> do
          fail "definite long: unimplemented"
