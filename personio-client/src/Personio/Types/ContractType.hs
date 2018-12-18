{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}
module Personio.Types.ContractType (
    ContractType (..),
    -- * Prisms
    _ContractType,
    -- * Conversion functions
    contractTypeToText,
    contractTypeFromText,
    ) where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import qualified Data.Csv as Csv

data ContractType
    = PermanentAllIn
    | Permanent
    | FixedTerm
  deriving stock (Eq, Ord, Show, Read, Typeable, Enum, Bounded, GhcGeneric, Lift)
  deriving anyclass (NFData, Hashable, Binary, SopGeneric, HasDatatypeInfo)
  deriving (Arbitrary) via (Sopica ContractType)
  deriving (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData, Csv.ToField, Csv.FromField, ToHtml) via (Enumica ContractType)

instance TextEnum ContractType where
    type TextEnumNames ContractType = '["permanent all-in", "permanent", "fixed term"]

contractTypeToText :: ContractType -> Text
contractTypeToText = enumToText

contractTypeFromText :: Text -> Maybe ContractType
contractTypeFromText = enumFromText

_ContractType :: Prism' Text ContractType
_ContractType = enumPrism

instance ToParamSchema ContractType where toParamSchema = enumToParamSchema
instance ToSchema ContractType where declareNamedSchema = enumDeclareNamedSchema
