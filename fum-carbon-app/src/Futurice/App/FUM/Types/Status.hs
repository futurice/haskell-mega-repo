{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Futurice.App.FUM.Types.Status (
    Status (..),
    -- * Prisms
    _Status,
    _StatusActive,
    _StatusSuspended,
    _StatusDeleted,
    -- * Conversion functions
    statusToText,
    statusFromText,
    ) where

import Futurice.Generics
import Futurice.Generics.Enum
import Futurice.Lucid.Foundation (ToHtml (..))
import Futurice.Prelude
import Prelude ()

import qualified Data.Csv as Csv

-- | User status
data Status
    = StatusActive
    | StatusSuspended  -- ^ temporary status.
    | StatusDeleted
  deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)
  deriving anyclass (NFData, Binary)

makePrisms ''Status
deriveGeneric ''Status
deriveLift ''Status

instance TextEnum Status where
    type TextEnumNames Status = '["active", "suspended", "deleted"]

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
