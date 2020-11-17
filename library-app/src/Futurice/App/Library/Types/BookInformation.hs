{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Library.Types.BookInformation where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Futurice.App.Sisosota.Types          (ContentHash)
import Futurice.Generics
import Futurice.IdMap                       (HasKey (..))
import Futurice.Prelude
import Prelude ()

newtype BookInformationId   = BookInformationId Int32 deriving newtype (Eq, Ord, Show, Read, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData, FromField, ToField)

data BookInformation = BookInformation
    { _bookInformationId          :: !BookInformationId
    , _bookTitle                  :: !Text
    , _bookISBN                   :: !Text
    , _bookAuthor                 :: !Text
    , _bookPublisher              :: !Text
    , _bookPublished              :: !Int
    , _bookCover                  :: !ContentHash
    , _bookInfoLink               :: !Text
    }
  deriving (Show, Typeable, GhcGeneric, FromRow, SopGeneric, HasDatatypeInfo)
  deriving (FromJSON, ToJSON) via (Sopica BookInformation)

deriveGeneric ''BookInformationId

makeLenses ''BookInformation

instance HasKey BookInformation where
    type Key BookInformation = BookInformationId
    key = bookInformationId

instance ToParamSchema BookInformationId where toParamSchema = newtypeToParamSchema
instance ToSchema BookInformationId where declareNamedSchema = newtypeDeclareNamedSchema
instance ToSchema BookInformation where declareNamedSchema = sopDeclareNamedSchema

instance FromRow BookInformationId where fromRow = field
