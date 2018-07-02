{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Library.Types.Library where

import Database.PostgreSQL.Simple.FromField as Postgres

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

data Library
    = Helsinki
    | Berlin
    | Munich
    | Stockholm
    | Tampere
    | Oslo
    | London
    | ELibrary
    | Unknown
    deriving (Eq, Show, Ord, Bounded, Enum, Typeable, Generic, Read)

deriveGeneric ''Library

instance TextEnum Library where
    type TextEnumNames Library =
        '["Helsinki"
         ,"Berlin"
         ,"Munich"
         ,"Stockholm"
         ,"Tampere"
         ,"Oslo"
         ,"London"
         ,"Elibrary"
         ,"Unknown"
         ]

instance FromField Library where
    fromField f mdata = return library
        where library = case mdata of
                Just "Helsinki" -> Helsinki
                Just "Berlin" -> Berlin
                Just "Munich" -> Munich
                Just "Stockholm" -> Stockholm
                Just "Tampere" -> Tampere
                Just "Oslo" -> Oslo
                Just "London" -> London
                Just "Elibrary" -> ELibrary
                _ -> Unknown

instance ToSchema Library where declareNamedSchema = enumDeclareNamedSchema

deriveVia [t| ToJSON Library `Via` Enumica Library |]
deriveVia [t| FromJSON Library `Via` Enumica Library |]
