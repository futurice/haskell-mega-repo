{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}
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
  deriving stock (Eq, Ord, Show, Read, Typeable, Enum, Bounded, GhcGeneric, Lift)
  deriving anyclass (NFData, Hashable, Binary, SopGeneric, HasDatatypeInfo)
  deriving (Arbitrary) via (Sopica SalaryType)
  deriving (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData, Csv.ToField, Csv.FromField, ToHtml) via (Enumica SalaryType)

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

instance ToParamSchema SalaryType where toParamSchema = enumToParamSchema
instance ToSchema SalaryType where declareNamedSchema = enumDeclareNamedSchema
