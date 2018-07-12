{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
module Futurice.App.Library.Types.BookInformationResponse where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.Types.BookInformation
import Futurice.App.Library.Types.Library

data Books = Books
    { _booksBookId  :: !BookId
    , _booksLibrary :: !Library
    }
    deriving (Eq, Ord, Show, Generic, ToJSON, ToSchema)

data BookInformationResponse = BookInformationResponse
    { _id          :: !BookInformationId
    , _title       :: !Text
    , _ISBN        :: !Text
    , _author      :: !Text
    , _publisher   :: !Text
    , _published   :: !Int
    , _cover       :: !Text
    , _amazonLink  :: !Text
    , _books       :: ![Books]
    } deriving  (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''BookInformationResponse

deriveVia [t| ToJSON BookInformationResponse `Via` Sopica BookInformationResponse |]

instance ToSchema BookInformationResponse
