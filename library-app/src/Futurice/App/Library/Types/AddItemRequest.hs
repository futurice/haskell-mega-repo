{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Futurice.App.Library.Types.AddItemRequest where

import Data.Text.Read (decimal)

import Futurice.Office   (offOther)
import Futurice.Prelude
import Prelude ()
import Servant.Multipart

import Futurice.App.Library.Types.Library

data AddBookInformation = AddBookInformation
    { _addBookTitle                  :: !Text
    , _addBookISBN                   :: !Text
    , _addBookAuthor                 :: !Text
    , _addBookPublisher              :: !Text
    , _addBookPublished              :: !Int
    , _addBookAmazonLink             :: !Text
    , _addBookLibraries              :: ![(Library, Int)]
    , _addBookCover                  :: !(FileData Mem)
    } deriving Show

data AddBoardGameInformation = AddBoardGameInformation
    { _addBoardgameName           :: !Text
    , _addBoardgamePublisher      :: !(Maybe Text)
    , _addBoardgamePublished      :: !(Maybe Int)
    , _addBoardgameDesigner       :: !(Maybe Text)
    , _addBoardgameArtist         :: !(Maybe Text)
    , _addBoardgameLibrary        :: ![(Library, Int)]
    }
    deriving Show

usedLibraries :: [Library]
usedLibraries = (filter (/= OfficeLibrary offOther) . filter (/= UnknownLibrary)) allLibraries

formtextToInt :: Text -> Maybe Int
formtextToInt t = case decimal t of
    Left _ -> Nothing
    Right (num,_) -> Just num

booksPerLibrary :: MultipartData a -> [(Library, Int)]
booksPerLibrary dt = catMaybes $ map (\l -> sequence (l, lookupInput ("amount-" <> libraryToText l) dt >>= formtextToInt)) $ usedLibraries

instance FromMultipart Mem AddBookInformation where
    fromMultipart multipartData = AddBookInformation
        <$> lookupInput "title" multipartData
        <*> lookupInput "isbn" multipartData
        <*> lookupInput "author" multipartData
        <*> lookupInput "publisher" multipartData
        <*> (lookupInput "published" multipartData >>=
              (\x -> case decimal x of
                       Left _ -> Nothing
                       Right (num,_) -> Just num))
        <*> lookupInput "amazon-link" multipartData
        <*> pure (booksPerLibrary multipartData)
        <*> cover
      where
        cover = lookupFile "cover-file" $ multipartData

instance FromMultipart Mem AddBoardGameInformation where
    fromMultipart multipartData = AddBoardGameInformation
        <$> lookupInput "name" multipartData
        <*> pure (lookupInput "publisher" multipartData)
        <*> pure (lookupInput "published" multipartData >>= formtextToInt)
        <*> pure (lookupInput "designer" multipartData)
        <*> pure (lookupInput "artist" multipartData)
        <*> pure (booksPerLibrary multipartData)
