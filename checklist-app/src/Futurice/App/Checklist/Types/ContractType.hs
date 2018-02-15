{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Futurice.App.Checklist.Types.ContractType where

import Futurice.Generics
import Futurice.Lucid.Generics
import Futurice.Prelude
import Prelude ()

import qualified Data.Csv as Csv

-- | Contract type affect what's need to be done.
data ContractType
    = ContractTypePermanent
    | ContractTypeExternal
    | ContractTypeFixedTerm
    | ContractTypePartTimer
    | ContractTypeSummerWorker
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)
  deriving anyclass (NFData, Binary)

makePrisms ''ContractType
deriveGeneric ''ContractType
deriveLift ''ContractType

instance TextEnum ContractType where
    type TextEnumNames ContractType =
        '["permanent"
        , "external"
        , "fixed-term"
        , "part-timer"
        , "summer-worker"
        ]

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

instance FieldToHtml ContractType
