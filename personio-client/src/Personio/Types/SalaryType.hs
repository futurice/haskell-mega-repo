{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Personio.Types.SalaryType (
    SalaryType (..),
    salaryTypeToText,
    salaryTypeFromText,
    _SalaryType,
    ) where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import qualified Data.Csv as Csv

data SalaryType
    = Monthly
    | Hourly
  deriving stock (Eq, Ord, Show, Read, Typeable, Enum, Bounded, Generic)
  deriving anyclass (NFData, Hashable, Binary)

makePrisms ''SalaryType
deriveGeneric ''SalaryType
deriveLift ''SalaryType

instance TextEnum SalaryType where
    type TextEnumNames SalaryType = '["Monthly", "Hourly"]

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------

salaryTypeToText :: SalaryType -> Text
salaryTypeToText = enumToText

salaryTypeFromText :: Text -> Maybe SalaryType
salaryTypeFromText = enumFromText

_SalaryType :: Prism' Text SalaryType
_SalaryType = enumPrism

deriveVia [t| Arbitrary SalaryType       `Via` Sopica SalaryType  |]
deriveVia [t| ToJSON SalaryType          `Via` Enumica SalaryType |]
deriveVia [t| FromJSON SalaryType        `Via` Enumica SalaryType |]
deriveVia [t| ToHttpApiData SalaryType   `Via` Enumica SalaryType |]
deriveVia [t| FromHttpApiData SalaryType `Via` Enumica SalaryType |]
deriveVia [t| Csv.ToField SalaryType     `Via` Enumica SalaryType |]
deriveVia [t| Csv.FromField SalaryType   `Via` Enumica SalaryType |]
deriveVia [t| ToHtml SalaryType          `Via` Enumica SalaryType |]

instance ToParamSchema SalaryType where toParamSchema = enumToParamSchema
instance ToSchema SalaryType where declareNamedSchema = enumDeclareNamedSchema
