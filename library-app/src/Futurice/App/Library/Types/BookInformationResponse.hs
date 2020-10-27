{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
module Futurice.App.Library.Types.BookInformationResponse where

import Futurice.App.Sisosota.Types (ContentHash)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.Types.BookInformation
import Futurice.App.Library.Types.Item
import Futurice.App.Library.Types.Library

data Books = Books
    { _booksLibrary :: !Library
    , _booksBookId  :: !ItemId
    }
    deriving (Eq, Ord, Show, GhcGeneric, ToSchema, Typeable, SopGeneric, HasDatatypeInfo)
    deriving (ToJSON, FromJSON) via (Sopica Books)

data BookInformationResponse = BookInformationResponse
    { _id          :: !BookInformationId
    , _title       :: !Text
    , _ISBN        :: !Text
    , _author      :: !Text
    , _publisher   :: !Text
    , _published   :: !Int
    , _cover       :: !ContentHash
    , _infoLink    :: !Text
    , _books       :: ![Books]
    } deriving (Show, Typeable, GhcGeneric, SopGeneric, HasDatatypeInfo)
      deriving (ToJSON, FromJSON) via (Sopica BookInformationResponse)

instance ToSchema BookInformationResponse
