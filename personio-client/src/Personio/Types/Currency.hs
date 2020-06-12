{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Personio.Types.Currency where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()


data Currency
    = EUR
    | GBP
    | NOK
    | SEK
    deriving (Eq, Show, Enum, Ord, Bounded, ToSchema, Generic, SopGeneric, NFData, Hashable)
    deriving Arbitrary via (Sopica Currency)
    deriving (ToJSON, FromJSON) via (Enumica Currency)

instance TextEnum Currency where
    type TextEnumNames Currency = '["EUR", "GBP", "NOK", "SEK"]

currencyFromText :: Text -> Maybe Currency
currencyFromText = enumFromText

currencyToText :: Currency -> Text
currencyToText = enumToText
