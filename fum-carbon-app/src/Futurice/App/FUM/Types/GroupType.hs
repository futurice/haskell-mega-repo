{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Futurice.App.FUM.Types.GroupType (
    GroupType (..),
    groupTypeToText,
    groupTypeFromText,
    -- * Prisms
    _GroupType,
    ) where

import Futurice.Generics
import Futurice.Lucid.Generics
import Futurice.Prelude
import Prelude ()

import qualified Data.Csv as Csv

data GroupType
    = GroupTypeAccess
    | GroupTypeProject
    | GroupTypeServer
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)
  deriving anyclass (NFData, Binary)

-- makeLenses ''GroupType -- we don't need prisms atm
deriveGeneric ''GroupType
-- TODO: remove when we drop support for GHC-7.10
deriveLift ''GroupType

instance TextEnum GroupType where
    type TextEnumNames GroupType = '["access", "project", "Server"]

-------------------------------------------------------------------------------
-- Boilerplate
-------------------------------------------------------------------------------

groupTypeToText :: GroupType -> Text
groupTypeToText = enumToText

groupTypeFromText :: Text -> Maybe GroupType
groupTypeFromText = enumFromText

_GroupType :: Prism' Text GroupType
_GroupType = enumPrism

deriveVia [t| Arbitrary GroupType       `Via` Sopica GroupType  |]
deriveVia [t| ToJSON GroupType          `Via` Enumica GroupType |]
deriveVia [t| FromJSON GroupType        `Via` Enumica GroupType |]
deriveVia [t| ToHttpApiData GroupType   `Via` Enumica GroupType |]
deriveVia [t| FromHttpApiData GroupType `Via` Enumica GroupType |]
deriveVia [t| Csv.ToField GroupType     `Via` Enumica GroupType |]
deriveVia [t| Csv.FromField GroupType   `Via` Enumica GroupType |]
deriveVia [t| ToHtml GroupType          `Via` Enumica GroupType |]

instance ToParamSchema GroupType where toParamSchema = enumToParamSchema
instance ToSchema GroupType where declareNamedSchema = enumDeclareNamedSchema

instance FieldToHtml GroupType
