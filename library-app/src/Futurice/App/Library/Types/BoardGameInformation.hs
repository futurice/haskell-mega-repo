{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Library.Types.BoardGameInformation where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

newtype BoardGameInformationId   = BoardGameInformationId Int32 deriving newtype (Eq, Ord, Show, FromJSON, ToJSON, ToHttpApiData, FromHttpApiData, FromField, ToField)

data BoardGameInformation = BoardGameInformation
    { _boardgameInformationId  :: !BoardGameInformationId
    , _boardgameName           :: !Text
    , _boardgamePublisher      :: !Text
    , _boardgamePublished      :: !Int
    , _boardgameDesigner       :: !Text
    , _boardgameArtist         :: !Text
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''BoardGameInformationId
deriveGeneric ''BoardGameInformation

makeLenses ''BoardGameInformation

deriveVia [t| ToJSON BoardGameInformation `Via` Sopica BoardGameInformation |]

instance ToParamSchema BoardGameInformationId where toParamSchema = newtypeToParamSchema
instance ToSchema BoardGameInformationId where declareNamedSchema = newtypeDeclareNamedSchema
instance ToSchema BoardGameInformation where declareNamedSchema = sopDeclareNamedSchema
