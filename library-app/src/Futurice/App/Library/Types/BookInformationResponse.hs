{-# LANGUAGE DataKinds       #-}
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
    deriving (Eq, Ord, Show, Generic, ToSchema, Typeable)

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
    } deriving  (Show, Typeable, Generic)

deriveGeneric ''Books
deriveGeneric ''BookInformationResponse

deriveVia [t| ToJSON BookInformationResponse `Via` Sopica BookInformationResponse |]
deriveVia [t| FromJSON BookInformationResponse `Via` Sopica BookInformationResponse |]
deriveVia [t| ToJSON Books `Via` Sopica Books |]
deriveVia [t| FromJSON Books `Via` Sopica Books |]

instance ToSchema BookInformationResponse
