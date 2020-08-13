{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Library.Types.AddItemRequest where

import Data.Char      (digitToInt, isDigit)
import Data.Text.Read (decimal)

import Futurice.Office   (offOther)
import Futurice.Prelude
import Prelude ()
import Servant.Multipart
import Text.Read         (readEither)

import Futurice.App.Library.Types.BoardGameInformation
import Futurice.App.Library.Types.BookInformation
import Futurice.App.Library.Types.Library

import qualified Data.ByteString.Lazy as DBL
import qualified Data.Text            as T

data CoverData = CoverData (FileData Mem)
               | CoverUrl Text
               deriving Show

data AddBookInformation = AddBookInformation
    { _addBookTitle                  :: !Text
    , _addBookISBN                   :: !Text
    , _addBookAuthor                 :: !Text
    , _addBookPublisher              :: !Text
    , _addBookPublished              :: !Int
    , _addBookInfoLink               :: !Text
    , _addBookLibraries              :: ![(Library, Int)]
    , _addBookCover                  :: !CoverData
    , _addBookInformationId          :: !(Maybe BookInformationId)
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

data AddItemRequest = AddItemRequest
    { _addItemLibrary                :: !Library
    , _addItemBookInformationId      :: !(Maybe BookInformationId)
    , _addItemBoardGameInformationId :: !(Maybe BoardGameInformationId)
    }
    deriving (Show, Generic)

-- Order alphabetically, but make sure Elibrary is last
librarySelectSortOrder :: Library -> Library -> Ordering
librarySelectSortOrder a b = case (a,b) of
                        (Elibrary, Elibrary) -> EQ
                        (Elibrary, _) -> GT
                        (_, Elibrary) -> LT
                        _ -> compare (libraryToText a) (libraryToText b)

usedLibraries :: [Library]
usedLibraries = let isKnownLibrary lib = (lib /= OfficeLibrary offOther) && (lib /= UnknownLibrary)
                    usedLibs = filter isKnownLibrary allLibraries
                in sortBy librarySelectSortOrder usedLibs

fromtextToInt :: Text -> Either String Int
fromtextToInt t = fst <$> decimal t

lookupInputAndClean :: Text -> MultipartData a -> Either String Text
lookupInputAndClean a b = T.strip <$> lookupInput a b

booksPerLibrary :: MultipartData a -> [(Library, Int)]
booksPerLibrary dt = catMaybes $ map (\l -> sequence (l, (either (const Nothing) Just (lookupInputAndClean ("amount-" <> libraryToText l) dt >>= fromtextToInt)))) $ usedLibraries

validateISBN :: Text -> Either String Text
validateISBN isbn = if all isDigit isbnString && isbncheck then Right isbn else Left "malformed isbn"
  where
    isbnString = T.unpack isbn
    isbncheck | length isbnString == 10 = isbn10check
              | length isbnString == 13 = isbn13check
              | otherwise = False
    isbn10check = mod (sum $ zipWith (\letter number -> digitToInt letter * number) isbnString (reverse [1..10])) 11 == 0
    isbn13check = mod (sum $ zipWith (\letter number -> digitToInt letter * number) isbnString (cycle [1,3])) 10 == 0

instance FromMultipart Mem AddBookInformation where
    fromMultipart multipartData = AddBookInformation
        <$> lookupInputAndClean "title" multipartData
        <*> (lookupInputAndClean "isbn" multipartData >>= (validateISBN . T.filter (/= '-')))
        <*> lookupInputAndClean "author" multipartData
        <*> lookupInputAndClean "publisher" multipartData
        <*> (lookupInputAndClean "published" multipartData >>= readEither . T.unpack)
        <*> lookupInputAndClean "info-link" multipartData
        <*> pure (booksPerLibrary multipartData)
        <*> maybe (Left "No cover data") Right cover
        <*> pure (either (const Nothing) Just $ BookInformationId <$> fromIntegral <$> (lookupInputAndClean "bookinformationid" multipartData >>= fromtextToInt))
      where
        isEmptyT t | T.null t = Nothing
                   | otherwise = Just t
        isEmptyB b | DBL.null (fdPayload b) = Nothing
                   | otherwise = Just b
        cover = (CoverUrl <$> ((either (const Nothing) Just $ lookupInput "cover-url" multipartData) >>= isEmptyT)) <|> (CoverData <$> ((either (const Nothing) Just $ lookupFile "cover-file" multipartData) >>= isEmptyB))

instance FromMultipart Mem AddBoardGameInformation where
    fromMultipart multipartData = AddBoardGameInformation
        <$> lookupInputAndClean "name" multipartData
        <*> pure (either (const Nothing) Just $ lookupInputAndClean "publisher" multipartData)
        <*> pure ((either (const Nothing) Just $ lookupInputAndClean "published" multipartData >>= fromtextToInt))
        <*> pure (either (const Nothing) Just $ lookupInputAndClean "designer" multipartData)
        <*> pure (either (const Nothing) Just $ lookupInputAndClean "artist" multipartData)
        <*> pure (booksPerLibrary multipartData)

instance FromMultipart Mem AddItemRequest where
    fromMultipart multipartData = AddItemRequest
        <$> (libraryFromText <$> lookupInputAndClean "library" multipartData)
        <*> pure ((either (const Nothing) Just $ lookupInputAndClean "bookinformationid" multipartData) >>= readMaybe . T.unpack)
        <*> pure ((either (const Nothing) Just $ lookupInputAndClean "boardgameinformationid" multipartData) >>= readMaybe . T.unpack)
