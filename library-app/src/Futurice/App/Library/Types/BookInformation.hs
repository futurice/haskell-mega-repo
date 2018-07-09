{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Library.Types.BookInformation where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

newtype BookId   = BookId Integer deriving newtype (Eq, Ord, Show, FromJSON, ToJSON, FromHttpApiData, FromField, ToField)
newtype BookInformationId   = BookInformationId Integer deriving newtype (Eq, Ord, Show, FromJSON, ToJSON, FromHttpApiData, FromField, ToField)

data BookInformation = BookInformation
    { _bookInformationId          :: !BookInformationId
    , _bookTitle                  :: !Text
    , _bookISBN                   :: !Text
    , _bookAuthor                 :: !Text
    , _bookPublisher              :: !Text
    , _bookPublished              :: !Int
    , _bookCover                  :: !Text
    , _bookAmazonLink             :: !Text
    }
  deriving (Eq, Ord, Show, Typeable, Generic, FromRow)

deriveGeneric ''BookId
deriveGeneric ''BookInformationId
deriveGeneric ''BookInformation

makeLenses ''BookInformation

deriveVia [t| ToJSON BookInformation `Via` Sopica BookInformation |]

instance ToParamSchema BookId where toParamSchema = newtypeToParamSchema
instance ToSchema BookId where declareNamedSchema = newtypeDeclareNamedSchema
instance ToParamSchema BookInformationId where toParamSchema = newtypeToParamSchema
instance ToSchema BookInformationId where declareNamedSchema = newtypeDeclareNamedSchema
instance ToSchema BookInformation where declareNamedSchema = sopDeclareNamedSchema
