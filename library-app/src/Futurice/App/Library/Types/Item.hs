{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
module Futurice.App.Library.Types.Item where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.Types.BoardGameInformation
import Futurice.App.Library.Types.BookInformation
import Futurice.App.Library.Types.Library

newtype ItemId   = ItemId Int32 deriving newtype (Eq, Ord, Show, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData, FromField, ToField)

data Item = Item
    { _itemId      :: !ItemId
    , _itemLibrary :: !Library
    , _itemInfo    :: !ItemInfo
    }
    deriving (Show, Generic)

data ItemInfo = ItemBook BookInformation
              | ItemBoardGame BoardGameInformation
              | ItemUnknown Text
              deriving (Show, Typeable, Generic)

deriveGeneric ''ItemId
deriveGeneric ''ItemInfo
deriveGeneric ''Item

instance ToJSON ItemInfo
instance ToJSON Item
instance ToParamSchema ItemId where toParamSchema = newtypeToParamSchema
instance ToSchema ItemId where declareNamedSchema = newtypeDeclareNamedSchema
instance ToSchema ItemInfo
instance ToSchema Item where declareNamedSchema = sopDeclareNamedSchema
