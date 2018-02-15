{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Futurice.Office (
    Office (..),
    officeToText,
    officeFromText,
    -- * Prisms
    _Office,
    ) where

import Futurice.Generics
import Futurice.Prelude
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
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)
  deriving anyclass (NFData, Binary)

-- makePrism ''Office
deriveGeneric ''Office
deriveLift ''Office

instance TextEnum Office where
    type TextEnumNames Office =
        '["Helsinki"
        , "Tampere"
        , "Berlin"
        , "London"
        , "Stockholm"
        , "Munich"
        , "Other"
        ]

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------

officeToText :: Office -> Text
officeToText = enumToText

officeFromText :: Text -> Maybe Office
officeFromText = enumFromText

_Office :: Prism' Text Office
_Office = enumPrism

deriveVia [t| Arbitrary Office       `Via` Sopica Office  |]
deriveVia [t| ToJSON Office          `Via` Enumica Office |]
deriveVia [t| FromJSON Office        `Via` Enumica Office |]
deriveVia [t| ToHttpApiData Office   `Via` Enumica Office |]
deriveVia [t| FromHttpApiData Office `Via` Enumica Office |]
deriveVia [t| Csv.ToField Office     `Via` Enumica Office |]
deriveVia [t| Csv.FromField Office   `Via` Enumica Office |]
deriveVia [t| ToHtml Office          `Via` Enumica Office |]

instance ToParamSchema Office where toParamSchema = enumToParamSchema
instance ToSchema Office where declareNamedSchema = enumDeclareNamedSchema
