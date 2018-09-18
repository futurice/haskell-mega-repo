{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Library.Types.BoardGameInformation where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Futurice.Generics
import Futurice.IdMap                       (HasKey (..))
import Futurice.Prelude
import Prelude ()

newtype BoardGameInformationId   = BoardGameInformationId Int32 deriving newtype (Eq, Ord, Show, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData, FromField, ToField)

data BoardGameInformation = BoardGameInformation
    { _boardGameInformationId  :: !BoardGameInformationId
    , _boardGameName           :: !Text
    , _boardGamePublisher      :: !(Maybe Text)
    , _boardGamePublished      :: !(Maybe Int)
    , _boardGameDesigner       :: !(Maybe Text)
    , _boardGameArtist         :: !(Maybe Text)
    }
    deriving (Eq, Ord, Show, Typeable, Generic, FromRow)

deriveGeneric ''BoardGameInformationId
deriveGeneric ''BoardGameInformation

makeLenses ''BoardGameInformation

deriveVia [t| ToJSON BoardGameInformation `Via` Sopica BoardGameInformation |]
deriveVia [t| FromJSON BoardGameInformation `Via` Sopica BoardGameInformation |]

instance HasKey BoardGameInformation where
    type Key BoardGameInformation = BoardGameInformationId
    key = boardGameInformationId
instance ToParamSchema BoardGameInformationId where toParamSchema = newtypeToParamSchema
instance ToSchema BoardGameInformationId where declareNamedSchema = newtypeDeclareNamedSchema
instance ToSchema BoardGameInformation where declareNamedSchema = sopDeclareNamedSchema
