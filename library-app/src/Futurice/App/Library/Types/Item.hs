{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
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

import qualified Database.PostgreSQL.Simple.FromRow as PQ

import Futurice.App.Library.Types.BoardGameInformation
import Futurice.App.Library.Types.BookInformation
import Futurice.App.Library.Types.ItemType
import Futurice.App.Library.Types.Library

-------------------------------------------------------------------------------
-- ItemId
-------------------------------------------------------------------------------

newtype ItemId = ItemId Int32
    deriving newtype (Eq, Ord, Show, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData, FromField, ToField)
    deriving stock (GhcGeneric)
    deriving anyclass (SopGeneric, HasDatatypeInfo)

instance ToParamSchema ItemId where toParamSchema = newtypeToParamSchema
instance ToSchema ItemId where declareNamedSchema = newtypeDeclareNamedSchema

-------------------------------------------------------------------------------
-- ItemInfo
-------------------------------------------------------------------------------

data ItemInfo ty where
    ItemBook :: BookInformation -> ItemInfo Book
    ItemBoardGame :: BoardGameInformation -> ItemInfo BoardGame

deriving instance Show (ItemInfo ty)
instance ShowP ItemInfo where showsPrecP = showsPrec

instance SItemTypeI ty => PQ.FromRow (ItemInfo ty) where
    fromRow = case sitemType :: SItemType ty of
        SBook      -> ItemBook <$> PQ.fromRow
        SBoardGame -> ItemBoardGame <$> PQ.fromRow

-------------------------------------------------------------------------------
-- Item
-------------------------------------------------------------------------------

data Item = Item
    { _itemId      :: !ItemId
    , _itemLibrary :: !Library
    , _itemInfo    :: !(Some ItemInfo)
    }
  deriving (Show, Generic)

makeLenses ''Item
deriveGeneric ''Item

instance HasKey Item where
    type Key Item = ItemId
    key = itemId

-- | Something...
instance ToSchema Item where declareNamedSchema = emptyDeclareNamedSchema
