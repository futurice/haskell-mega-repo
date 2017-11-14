module Neleus.Types where

import Data.ByteString (ByteString)

-------------------------------------------------------------------------------
-- Value
-------------------------------------------------------------------------------

-- | ASN.1 Value
data ASN1
    = EOC                        -- ^ 0x00
    | Bool Bool                  -- ^ 0x01
    | Int Integer                -- ^ 0x02
    -- | BitString ByteString       -- ^ 0x03
    | OctetString ByteString     -- ^ 0x04
    | Null                       -- ^ 0x05
    -- oid -- ^ 0x06
    -- object descriptor -- ^ 0x07
    -- external type -- ^ 0x08
    -- Real Double -- ^ 0x09
    | Enum Integer               -- ^ 0x0A
    -- embedded-pdv type  0x0b
    -- relative oid 0x0c
    -- 0xd / 0xef reserved for future editions of X.680
    | Sequence [ASN1]            -- ^ 0x10
    -- 18-22,25-30 Character String types
    -- 23,24 TimeTypes

    -- Non universal types
    | Application Tag BS
    | Context Tag BS
    | Private Tag BS
  deriving (Show, Eq, Ord)

data BS
    = BS ByteString  -- ^ primitive value
    | Val [ASN1]     -- ^ constructed value
  deriving (Show, Eq, Ord)

-------------------------------------------------------------------------------
-- Identifier
-------------------------------------------------------------------------------

-- | Element class
data Class
    = UniversalC
    | ApplicationC
    | ContextC
    | PrivateC
  deriving (Show, Eq, Ord, Enum, Bounded)

data PC
    = Primitive
    | Constructed
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Tag
type Tag = Word

-- |  Length can we finite: 'Just', or indefinite 'Nothing'.
type Length = Maybe Word

-- | Identifier with the class, tag, constructed flag.
data Identifier = Identifier !Class !PC !Tag
  deriving (Show, Eq)
