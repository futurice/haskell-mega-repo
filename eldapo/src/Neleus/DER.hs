{-# LANGUAGE MultiWayIf, DeriveFunctor #-}
-- |
--
-- TODO: max length of value in parsers?
module Neleus.DER (
    -- * Distinguished encoding rules
    DER (..),
    stepDER,
    fullDER,
    stepParser,
    -- * recursion-schemes
    Fix (..),
    unfix,
    ) where

import Data.Bits             ((.&.))
import Data.Functor.Foldable (Fix (..), unfix)

import qualified Data.Attoparsec.ByteString      as A
import qualified Data.Attoparsec.ByteString.Lazy as AL
import qualified Data.ByteString                 as BS
import qualified Data.ByteString.Lazy            as LBS

-- TODO: rewrite without attoparsec

import Neleus.Types

-- | A stream of data values in DER.
--
-- See X.690 (08/2015) __10 Distinguished encoding rules__.
--
-- * __10.1__ Length forms: only definite forms
--
-- * __10.2__ String encoding forms: only primitive en coding
--
-- * __10.3__ Set components in an order of their tags.
--
-- This is easier format to parse than BER.
--
-- The type is paramterised over a type of tail of the stream.
-- @'DER' 'LBS.ByteString'@ let us lazily process lazy 'LBS.ByteString',
-- On the other hand, we can parse strict 'BS.ByteString' into @'Fix' 'DER'@.
--
data DER s
    = Fin {-# UNPACK #-} !Identifier !BS.ByteString s -- todo add Word back
      -- ^ identifier, contents, rest.
      --
      -- /Note:/ length is in the 'BS.ByteString'.
    | Err String -- TODO: better error type.
      -- ^ Error
    | End
      -- ^ End-of-stream
  deriving (Show, Functor)

-- TODO: Show1 DER

-- | Parse single DER value from a lazy 'LBS.ByteString'.
stepDER :: LBS.ByteString -> DER LBS.ByteString
stepDER lbs
    | LBS.null lbs = End
    | otherwise = case AL.parse stepParser lbs of
        AL.Fail _ _ err                -> Err err
        AL.Done lbs' (ident, contents) -> Fin ident contents lbs'

-- | Parse full stream of 'DER' values from a strict 'BS.ByteString'.
fullDER :: BS.ByteString -> Fix DER
fullDER bs
    | BS.null bs = Fix End
    | otherwise = Fix $ case A.parse stepParser bs of
        A.Fail _ _ err               -> Err err
        A.Partial _                  -> Err "partial"
        A.Done bs' (ident, contents) -> Fin ident contents (fullDER bs')

-- | Attoparsec parser for the header, i.e. identifier and length octets of the value.
stepParser :: A.Parser (Identifier, BS.ByteString)
stepParser = do
    -- identifier octets
    ident <- identifierParser
    -- length octets
    len <- lengthParser
    case len of
        Nothing -> fail "DER: definite form of length encoding shall be used"
        Just len' -> do
            -- content octets
            bs <- AL.take (fromIntegral len')
            return (ident, bs)

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
