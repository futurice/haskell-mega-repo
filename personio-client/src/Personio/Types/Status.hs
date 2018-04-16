{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
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
  deriving stock (Eq, Ord, Show, Read, Typeable, Enum, Bounded, Generic)
  deriving anyclass (NFData, Hashable, Binary)

-- makePrisms ''Status
deriveGeneric ''Status
deriveLift ''Status

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

deriveVia [t| Arbitrary Status       `Via` Sopica Status  |]
deriveVia [t| ToJSON Status          `Via` Enumica Status |]
deriveVia [t| FromJSON Status        `Via` Enumica Status |]
deriveVia [t| ToHttpApiData Status   `Via` Enumica Status |]
deriveVia [t| FromHttpApiData Status `Via` Enumica Status |]
deriveVia [t| Csv.ToField Status     `Via` Enumica Status |]
deriveVia [t| Csv.FromField Status   `Via` Enumica Status |]
deriveVia [t| ToHtml Status          `Via` Enumica Status |]

instance ToParamSchema Status where toParamSchema = enumToParamSchema
instance ToSchema Status where declareNamedSchema = enumDeclareNamedSchema
