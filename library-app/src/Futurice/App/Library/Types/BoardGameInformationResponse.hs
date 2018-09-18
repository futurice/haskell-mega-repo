{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Futurice.App.Library.Types.BoardGameInformationResponse where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.Types.BoardGameInformation
import Futurice.App.Library.Types.Item
import Futurice.App.Library.Types.Library

data BoardGames = BoardGames
    { _boardGamesLibrary      :: !Library
    , _boardGamesBoardGameId  :: !ItemId
    }
    deriving (Eq, Ord, Show, Generic, ToSchema, Typeable)

data BoardGameInformationResponse = BoardGameInformationResponse
    { _boardgameResponseInformationId  :: !BoardGameInformationId
    , _boardgameResponseName           :: !Text
    , _boardgameResponsePublisher      :: !(Maybe Text)
    , _boardgameResponsePublished      :: !(Maybe Int)
    , _boardgameResponseDesigner       :: !(Maybe Text)
    , _boardgameResponseArtist         :: !(Maybe Text)
    , _boardgameResponseGames          :: ![BoardGames]
    }
    deriving (Show, Typeable, Generic)

deriveGeneric ''BoardGameInformationResponse

makeLenses ''BoardGameInformationResponse
