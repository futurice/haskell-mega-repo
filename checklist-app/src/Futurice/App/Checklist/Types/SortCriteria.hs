{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
module Futurice.App.Checklist.Types.SortCriteria where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

-- | Helper type for stats page sorting
data SortCriteria = SortByActiveFuture | SortByActivePast | SortByArchive | SortByBoth
  deriving (Eq, Show, Bounded, Enum, Generic)

deriveGeneric ''SortCriteria
deriveVia [t| ToHttpApiData SortCriteria `Via` Enumica SortCriteria |]
deriveVia [t| FromHttpApiData SortCriteria `Via` Enumica SortCriteria |]

instance TextEnum SortCriteria where
    type TextEnumNames SortCriteria = '["activefuture", "activepast", "archive", "both"]

instance ToParamSchema SortCriteria where toParamSchema = enumToParamSchema
instance ToSchema SortCriteria where declareNamedSchema = enumDeclareNamedSchema
