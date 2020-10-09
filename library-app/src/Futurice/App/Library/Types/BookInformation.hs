{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Library.Types.BookInformation where

import Data.Aeson                           (withText)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Futurice.App.Sisosota.Types          (ContentHash)
import Futurice.Generics
import Futurice.IdMap                       (HasKey (..))
import Futurice.Prelude
import Prelude ()

import qualified Data.Text as T

newtype BookInformationId   = BookInformationId Int32 deriving newtype (Eq, Ord, Show, Read, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData, FromField, ToField)

data Language = English
              | Finnish
              | German
              | Other
              deriving (Show, GhcGeneric, SopGeneric, ToSchema, Enum, Bounded)

instance TextEnum Language where
    type TextEnumNames Language = '["English", "Finnish", "German", "Other"]

instance FromField Language where
    fromField _ mlang =
      case mlang of
        Nothing -> mzero
        Just lan -> maybe mzero pure $ enumFromText $ decodeUtf8Lenient lan

instance ToField Language where
    toField = toField . enumToText

instance ToJSON Language where
    toJSON = toJSON . enumToText

instance FromJSON Language where
    parseJSON = withText "Language" $ \language ->
        maybe mzero pure $ enumFromText language

data Category = CategoryTech
              | CategoryStrategyAndCulture
              | CategoryDesign
              | CategoryOther
              deriving (GhcGeneric, SopGeneric, ToSchema, Enum, Bounded)

instance TextEnum Category where
    type TextEnumNames Category = '["Tech", "Strategy and Culture", "Design", "Other"]

instance Show Category where
    show = T.unpack . enumToText

instance FromField Category where
    fromField _ mcategory =
      case mcategory of
        Nothing -> mzero
        Just cat -> maybe mzero pure $ enumFromText $ decodeUtf8Lenient cat

instance ToField Category where
    toField = toField . enumToText

instance ToJSON Category where
  toJSON = toJSON . enumToText

instance FromJSON Category where
  parseJSON = withText "Category" $ \category ->
    maybe mzero pure $ enumFromText category

data BookInformation = BookInformation
    { _bookInformationId          :: !BookInformationId
    , _bookTitle                  :: !Text
    , _bookISBN                   :: !Text
    , _bookAuthor                 :: !Text
    , _bookPublisher              :: !Text
    , _bookPublished              :: !Int
    , _bookCover                  :: !ContentHash
    , _bookInfoLink               :: !Text
    , _bookLanguage               :: !(Maybe Language)
    , _bookCategory               :: !(Maybe Category)
    }
  deriving (Show, Typeable, Generic, FromRow)

deriveGeneric ''BookInformationId
deriveGeneric ''BookInformation

makeLenses ''BookInformation

deriveVia [t| ToJSON BookInformation `Via` Sopica BookInformation |]
deriveVia [t| FromJSON BookInformation `Via` Sopica BookInformation |]

instance HasKey BookInformation where
    type Key BookInformation = BookInformationId
    key = bookInformationId

instance ToParamSchema BookInformationId where toParamSchema = newtypeToParamSchema
instance ToSchema BookInformationId where declareNamedSchema = newtypeDeclareNamedSchema
instance ToSchema BookInformation where declareNamedSchema = sopDeclareNamedSchema

instance FromRow BookInformationId where fromRow = field
