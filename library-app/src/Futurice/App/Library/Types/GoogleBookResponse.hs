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
import Futurice.Generics
import Futurice.Prelude
import Prelude ()
import Data.Time.Calendar (toGregorian)

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
    , _gbrPublisher :: !(Maybe Text)
    , _gbrPublished :: !Int
    , _gbrBooksLink :: !Text
    , _gbrCoverLink :: !Text
    , _gbrSelfLink  :: !Text
    }

makeLenses ''GoogleBookResponse

instance FromJSON GoogleBookResponse where
    parseJSON = withObject "google-book-response" $ \o -> do
        maybeBook <- listToMaybe <$> o .: "items"
        let book = fromMaybe mempty maybeBook
        vi <- book .: "volumeInfo"
        il <- vi .: "imageLinks"
        let parseYear = do
                Just year <- readMaybe <$> vi .: "publishedDate"
                pure year
        GoogleBookResponse  <$> vi .: "title"
                            <*> vi .: "industryIdentifiers"
                            <*> vi .: "authors"
                            <*> vi .:? "publisher"
                            <*> (((\(y,_,_) -> fromIntegral y) . toGregorian <$> vi .: "publishedDate") <|> parseYear)
                            <*> vi .: "canonicalVolumeLink"
                            <*> il .: "thumbnail"
                            <*> book .: "selfLink"

data GoogleIdBookResponse = GoogleIdBookResponse
    { _gibrId        :: !Text
    , _gibrPublisher :: !(Maybe Text)
    }

makeLenses ''GoogleIdBookResponse

instance FromJSON GoogleIdBookResponse where
    parseJSON = withObject "google-id-book-response" $ \book -> do
        vi <- book .: "volumeInfo"
        GoogleIdBookResponse
            <$> book .: "id"
            <*> vi .:? "publisher"
