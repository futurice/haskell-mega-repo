{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Futurice.App.Library.Types.EditItemRequest where

import Futurice.Prelude
import Prelude ()
import Servant.Multipart

import Futurice.App.Library.Types.AddItemRequest
import Futurice.App.Library.Types.BookInformation

import qualified Data.Text as T

data EditBookInformation = EditBookInformation
    { _editBookInformationId          :: !BookInformationId
    , _editBookTitle                  :: !Text
    , _editBookISBN                   :: !Text
    , _editBookAuthor                 :: !Text
    , _editBookPublisher              :: !Text
    , _editBookPublished              :: !Int
    , _editBookAmazonLink             :: !Text
    } deriving Show

deriveGeneric ''EditBookInformation
makeLenses ''EditBookInformation

instance FromMultipart Mem EditBookInformation where
    fromMultipart multipartData = EditBookInformation
        <$> (BookInformationId <$> fromIntegral <$> (lookupInputAndClean "bookinformationid" multipartData >>= fromtextToInt))
        <*> lookupInputAndClean "title" multipartData
        <*> (lookupInputAndClean "isbn" multipartData >>= (validateISBN . T.filter (/= '-')))
        <*> lookupInputAndClean "author" multipartData
        <*> lookupInputAndClean "publisher" multipartData
        <*> (lookupInputAndClean "published" multipartData >>= readMaybe . T.unpack)
        <*> lookupInputAndClean "amazon-link" multipartData
