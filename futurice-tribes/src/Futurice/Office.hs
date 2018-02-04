{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.Office (
    Office (..),
    officeToText,
    officeFromText,
    -- * Prisms
    _Office,
    ) where

import Futurice.Generics
import Futurice.Generics.Enum
import Futurice.Prelude
import Lucid                  (ToHtml (..))
import Prelude ()

import qualified Data.Csv as Csv

data Office
    = OffHelsinki
    | OffTampere
    | OffBerlin
    | OffLondon
    | OffStockholm
    | OffMunich
    | OffOther
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)

-- makeLenses ''Office -- we don't need prisms atm
deriveGeneric ''Office
-- TODO: remove when we drop support for GHC-7.10
deriveLift ''Office

ei :: EnumInstances Office
ei = sopEnumInstances $
    K "Helsinki" :*
    K "Tampere" :*
    K "Berlin" :*
    K "London" :*
    K "Stockholm" :*
    K "Munich" :*
    K "Other" :*
    Nil

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------

officeToText :: Office -> Text
officeToText = enumToText ei

officeFromText :: Text -> Maybe Office
officeFromText = enumFromText ei

_Office :: Prism' Text Office
_Office = enumPrism ei

instance NFData Office
instance Binary Office

instance Arbitrary Office where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance ToHtml Office where
    toHtmlRaw = toHtml
    toHtml = toHtml . enumToText ei

instance ToParamSchema Office where
    toParamSchema = enumToParamSchema ei

instance ToSchema Office where
    declareNamedSchema = enumDeclareNamedSchema ei

instance ToJSON Office where
    toJSON = enumToJSON ei

instance FromJSON Office where
    parseJSON = enumParseJSON ei

instance FromHttpApiData Office where
    parseUrlPiece = enumParseUrlPiece ei

instance ToHttpApiData Office where
    toUrlPiece = enumToUrlPiece ei

instance Csv.ToField Office where
    toField = enumCsvToField ei

instance Csv.FromField Office where
    parseField = enumCsvParseField ei
