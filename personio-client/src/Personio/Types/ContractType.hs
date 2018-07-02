{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Personio.Types.ContractType (
    ContractType (..),
    -- * Prisms
    _PermanentAllIn,
    _Permanent,
    _FixedTerm,
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
  deriving stock (Eq, Ord, Show, Read, Typeable, Enum, Bounded, Generic)
  deriving anyclass (NFData, Hashable, Binary)

makePrisms ''ContractType
deriveGeneric ''ContractType
deriveLift ''ContractType

instance TextEnum ContractType where
    type TextEnumNames ContractType = '["permanent all-in", "permanent", "fixed term"]

contractTypeToText :: ContractType -> Text
contractTypeToText = enumToText

contractTypeFromText :: Text -> Maybe ContractType
contractTypeFromText = enumFromText

_ContractType :: Prism' Text ContractType
_ContractType = enumPrism

deriveVia [t| Arbitrary ContractType       `Via` Sopica ContractType  |]
deriveVia [t| ToJSON ContractType          `Via` Enumica ContractType |]
deriveVia [t| FromJSON ContractType        `Via` Enumica ContractType |]
deriveVia [t| ToHttpApiData ContractType   `Via` Enumica ContractType |]
deriveVia [t| FromHttpApiData ContractType `Via` Enumica ContractType |]
deriveVia [t| Csv.ToField ContractType     `Via` Enumica ContractType |]
deriveVia [t| Csv.FromField ContractType   `Via` Enumica ContractType |]
deriveVia [t| ToHtml ContractType          `Via` Enumica ContractType |]

instance ToParamSchema ContractType where toParamSchema = enumToParamSchema
instance ToSchema ContractType where declareNamedSchema = enumDeclareNamedSchema
