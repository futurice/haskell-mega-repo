{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Library.Types.Item where

import Data.Aeson
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Futurice.Generics
import Futurice.IdMap                       (HasKey (..))
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.Types.BoardGameInformation
import Futurice.App.Library.Types.BookInformation
import Futurice.App.Library.Types.Library

newtype ItemId = ItemId Int32
    deriving newtype (Eq, Ord, Show, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData, FromField, ToField)

data Item = Item
    { _itemId      :: !ItemId
    , _itemLibrary :: !Library
    , _itemInfo    :: !ItemInfo
    }
    deriving (Show, Generic)

data ItemInfo = ItemBook BookInformation
              | ItemBoardGame BoardGameInformation
              deriving (Show, Typeable, Generic)

makeLenses ''Item

deriveGeneric ''ItemId
deriveGeneric ''ItemInfo
deriveGeneric ''Item

instance HasKey Item where
    type Key Item = ItemId
    key = itemId
instance ToJSON ItemInfo
instance ToJSON Item
instance ToParamSchema ItemId where toParamSchema = newtypeToParamSchema
instance ToSchema ItemId where declareNamedSchema = newtypeDeclareNamedSchema
instance ToSchema ItemInfo
instance ToSchema Item where declareNamedSchema = sopDeclareNamedSchema
instance FromJSON ItemInfo where
    parseJSON = withObject "Item" $ \item -> do
        book <- item .:? "book"
        boardgame <- item .:? "boardgame"
        case book of
          Just b -> pure $ ItemBook b
          Nothing -> case boardgame of
            Just bo -> pure $ ItemBoardGame bo
            Nothing -> empty
