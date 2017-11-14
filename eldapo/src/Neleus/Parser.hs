{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}
module Neleus.Parser where

import Data.Bifunctor     (first)
import Data.List.NonEmpty (NonEmpty (..))

import qualified Data.ByteString as BS

import Neleus.DER
import Neleus.Integer
import Neleus.Types

import qualified Data.ByteString.Base16 as BS16
import           Debug.Trace

data P s a where
    -- Don't consume anything, we are done.
    -- Done :: a -> P s a

    -- Process a raw value, given a parsed identifier and length.
    ConsumeRawValue :: (Identifier -> Word -> Action a) -> P s a

    -- ConsumeValue    :: P s a -> (a -> P s b) -> P s b

    -- Parse values until the end.
    SequenceOf   :: ([x] -> xs) -> P s x -> P s xs

instance Functor (P s) where
    fmap f (ConsumeRawValue k) = ConsumeRawValue (\ident len -> fmap f (k ident len))
    fmap f (SequenceOf k p) = SequenceOf (f . k) p

-- TODO: Apply (P s)

data Action a
    = Recurse (P (Fix DER) a)
    -- ^ Recurse value, parse using 'P'.
    | Fail String
    -- ^ Fail parse

    -- Check       :: Bool -> String -> P s a -> P' s a

    -- primitives
    | ConsumeEmpty a
    -- ^ Parse End-of-content, Null
    | ConsumeBool (Bool -> a)
    -- ^ Parse Boolean
    | ConsumeInt (Integer -> a)
    -- ^ Parse Integer, Enum
    | ConsumeOctetString (BS.ByteString -> a)
    -- ^ Parse Octet String
  deriving (Functor)

decode
    :: (s -> DER s)          -- ^ a way to consume stream
    -> P s a                 -- ^ parser
    -> s                     -- ^ stream
    -> Either Error (a, s)   -- ^ result is either an error, or value and rest of the stream.
decode step (ConsumeRawValue f) s = case step s of
    End -> Left "unexpected end-of-input"
    Err err ->  Left err
    Fin ident contents s' ->
        (,s') <$> decodeAction ident contents (f ident $ fromIntegral $ BS.length contents)
decode  step (SequenceOf f p) s =
    first f <$> decodeMany step p s

decodeAction
    :: Identifier
    -> BS.ByteString
    -> Action a
    -> Either Error a
decodeAction  (Identifier _cls pc _tag) bs action = case action of
    Fail err -> Left err
    Recurse p -> do
        requireConstructed pc
        let ders = fullDER bs
        case decode unfix p $ traceShow (BS16.encode bs) ders of
            Right (x, Fix End)       -> Right x
            Right (_, Fix (Err err)) -> Left err
            Right (_, Fix _)         -> Left "not all input consumed"
            Left err                 -> Left err

    -- 8.1.5 End-of-contents octets
    -- 8.8 Encoding of a null value
    ConsumeEmpty x -> do
        requirePrimitive pc
        requireBool (len == 0) "End-of-contents content's should be absent"
        return x

    -- 8.2 Encoding of a boolean value
    ConsumeBool f -> do
        -- 8.2.1 boolean should be primitive
        requirePrimitive pc
        -- should consist of a single octet
        requireBool (len == 1) "Bool: shall consist of a single octet"
        let w = BS.head bs
        -- 8.2.2 False encoded as 0, True as everything else
        return $ f $ w /= 0

    -- 8.7 Encoding of an octetstring value
    ConsumeOctetString f -> do
        -- DER 10.2: Primitive form should be used
        requirePrimitive pc
        return (f bs)

    -- 8.3 Encoding of an integer value
    -- Note: 8.4 Encoding of an enumerated value, encoded as an integer value
    ConsumeInt f -> do
        -- 8.3.1 integer should be primitive
        requirePrimitive pc
        -- 8.3.1 should be one or more octets
        requireBool (len >= 1) "Integer: Shall consist >=1 octets"
        case BS.unpack bs of
            (w:ws) -> return (f (decodeInteger (w :| ws)))
            []     -> Left "panic! len >= 1, but empty contents"

  where
    len = BS.length bs

decodeMany
    :: (s -> DER s)
    -> P s a
    -> s
    -> Either Error ([a], s)
decodeMany step p s0 = case step s0 of
    End -> return ([], s0)
    _   -> do
        (x, s1) <- decode step p s0
        (xs, s2) <- decodeMany step p s1
        return (x : xs, s2)

-------------------------------------------------------------------------------
-- ASN1 Parser
-------------------------------------------------------------------------------

asn1Parser :: P s ASN1
asn1Parser = ConsumeRawValue $ \ident@(Identifier cls pc tag) _len ->
    case cls of
        UniversalC -> case traceShow ident tag of
            0x00 -> ConsumeEmpty EOC
            0x01 -> ConsumeBool Bool
            0x02 -> ConsumeInt Int
            0x04 -> ConsumeOctetString OctetString
            0x05 -> ConsumeEmpty Null
            0x0A -> ConsumeInt Enum
            0x10 -> Recurse (SequenceOf Sequence asn1Parser)
            _    -> Fail $ "Unimplemented tag" ++ show ident
        ApplicationC -> nonUniversalParser pc $ Application tag
        ContextC     -> nonUniversalParser pc $ Context tag
        PrivateC     -> nonUniversalParser pc $ Private tag

nonUniversalParser :: PC -> (BS -> ASN1) -> Action ASN1
nonUniversalParser Constructed f = Recurse (SequenceOf (f . Val) asn1Parser)
nonUniversalParser Primitive   f = ConsumeOctetString (f . BS)

-------------------------------------------------------------------------------
-- Combinators
-------------------------------------------------------------------------------

-- | Runs the parser on the stream and embeds into other parser.
recurse :: P s a -> s -> P s' a
recurse = error "implement me"

untilEnd :: P s a -> P s [a]
untilEnd = error "implement me"

-------------------------------------------------------------------------------
-- Primitives
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- Guards
-------------------------------------------------------------------------------

requireBool :: Bool -> String -> Either String ()
requireBool True _    = Right ()
requireBool False err = Left err

requirePrimitive :: PC -> Either String ()
requirePrimitive Primitive   = Right ()
requirePrimitive Constructed = Left "Primitive encoding expected"

requireConstructed :: PC -> Either String ()
requireConstructed Constructed = Right ()
requireConstructed Primitive   = Left "Constructed encoding expected"

-------------------------------------------------------------------------------
-- Error type
-------------------------------------------------------------------------------

type Error = String
