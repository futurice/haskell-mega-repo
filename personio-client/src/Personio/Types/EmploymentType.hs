{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}
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
  deriving stock (Eq, Ord, Show, Read, Typeable, Enum, Bounded, GhcGeneric, Lift)
  deriving anyclass (NFData, Hashable, Binary, SopGeneric, HasDatatypeInfo)
  deriving (Arbitrary) via (Sopica EmploymentType)
  deriving (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData, Csv.ToField, Csv.FromField, ToHtml) via (Enumica EmploymentType)

-- makePrisms ''EmploymentType

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

instance ToParamSchema EmploymentType where toParamSchema = enumToParamSchema
instance ToSchema EmploymentType where declareNamedSchema = enumDeclareNamedSchema
