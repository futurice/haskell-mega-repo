{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DerivingVia       #-}
module Futurice.App.Library.Types.GoogleBookResponse where

import Control.Lens
import Data.Aeson
import Futurice.Aeson
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

data ISBNType 
    = ISBN10
    | ISBN13

instance FromJSON ISBNType where
    parseJSON (String "ISBN_10") = pure ISBN10
    parseJSON (String "ISBN_13") = pure ISBN13
    parseJSON _                  = mzero

data ISBN = ISBN
    { _isbnType :: !ISBNType 
    , _isbnIdentifier :: !Text
    } deriving stock (GhcGeneric)
      deriving anyclass (SopGeneric, HasDatatypeInfo)
      deriving (FromJSON) via (Sopica ISBN)

data GoogleBookResponse = GoogleBookResponse
    { _gbrTitle     :: !Text
    , _gbrISBNs     :: ![ISBN]
    , _gbrAuthors   :: ![Text]
    , _gbrPublisher :: !Text
    , _gbrPublished :: !Int
    , _gbrBooksLink :: !Text
    , _gbrCoverLink :: !Text
    }

makeLenses ''GoogleBookResponse

instance FromJSON GoogleBookResponse where
    parseJSON (Object o) = do
        maybeBook <- listToMaybe <$> o .: "items"
        let book = fromMaybe mempty maybeBook
        vi <- book .: "volumeInfo"
        il <- vi .: "imageLinks"
        GoogleBookResponse  <$> vi .: "title"
                            <*> vi .: "industryIdentifiers"
                            <*> vi .: "authors"
                            <*> vi .: "publisher"
                            <*> (getParsedAsIntegral <$> vi .: "publishedDate")
                            <*> vi .: "canonicalVolumeLink"
                            <*> il .: "thumbnail"
    parseJSON _ = mzero