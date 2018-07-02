{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ImpredicativeTypes  #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Personio.Types.EmploymentType (
    EmploymentType (..),
    employmentTypeToText,
    employmentTypeFromText,
    _EmploymentType,
    ) where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import qualified Data.Csv as Csv

data EmploymentType
    = Internal
    | External
  deriving stock (Eq, Ord, Show, Read, Typeable, Enum, Bounded, Generic)
  deriving anyclass (NFData, Hashable, Binary)

makePrisms ''EmploymentType
deriveGeneric ''EmploymentType
deriveLift ''EmploymentType

instance TextEnum EmploymentType where
    type TextEnumNames EmploymentType = '["internal", "external"]

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------

employmentTypeToText :: EmploymentType -> Text
employmentTypeToText = enumToText

employmentTypeFromText :: Text -> Maybe EmploymentType
employmentTypeFromText = enumFromText

_EmploymentType :: Prism' Text EmploymentType
_EmploymentType = enumPrism

deriveVia [t| Arbitrary EmploymentType       `Via` Sopica EmploymentType  |]
deriveVia [t| ToJSON EmploymentType          `Via` Enumica EmploymentType |]
deriveVia [t| FromJSON EmploymentType        `Via` Enumica EmploymentType |]
deriveVia [t| ToHttpApiData EmploymentType   `Via` Enumica EmploymentType |]
deriveVia [t| FromHttpApiData EmploymentType `Via` Enumica EmploymentType |]
deriveVia [t| Csv.ToField EmploymentType     `Via` Enumica EmploymentType |]
deriveVia [t| Csv.FromField EmploymentType   `Via` Enumica EmploymentType |]
deriveVia [t| ToHtml EmploymentType          `Via` Enumica EmploymentType |]

instance ToParamSchema EmploymentType where toParamSchema = enumToParamSchema
instance ToSchema EmploymentType where declareNamedSchema = enumDeclareNamedSchema
