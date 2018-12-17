{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}
module Personio.Types.Status (
    Status(..),
    statusToText,
    statusFromText,
    _Status,
    ) where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import qualified Data.Csv as Csv

-- | Employee contractual status with initial state being Onboarding
-- Personio changes status to Inactive on reaching Employee.endDate
data Status
    = Active
    | Inactive
    | Onboarding
    | Leave
  deriving stock (Eq, Ord, Show, Read, Typeable, Enum, Bounded, GhcGeneric, Lift)
  deriving anyclass (NFData, Hashable, Binary, SopGeneric, HasDatatypeInfo)
  deriving (Arbitrary) via (Sopica Status)
  deriving (ToJSON, FromJSON, ToHttpApiData, FromHttpApiData, Csv.ToField, Csv.FromField, ToHtml) via (Enumica Status)

instance TextEnum Status where
    type TextEnumNames Status = '["Active", "Inactive", "Onboarding", "Leave"]

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------

statusToText :: Status -> Text
statusToText = enumToText

statusFromText :: Text -> Maybe Status
statusFromText = enumFromText

_Status :: Prism' Text Status
_Status = enumPrism

instance ToParamSchema Status where toParamSchema = enumToParamSchema
instance ToSchema Status where declareNamedSchema = enumDeclareNamedSchema
