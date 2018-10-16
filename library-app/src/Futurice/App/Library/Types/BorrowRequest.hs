{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Library.Types.BorrowRequest where

import Data.Aeson
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.Types.BookInformation
import Futurice.App.Library.Types.Library

data BorrowRequest = BorrowRequest
    { _borrowBook      :: !BookInformationId
    , _borrowLibrary   :: !Library
    }
    deriving (Show, Typeable)

deriveGeneric ''BorrowRequest

instance FromJSON BorrowRequest where
    parseJSON (Object v) =  BorrowRequest
        <$> v .: "book"
        <*> (libraryFromText <$> v .: "library")
    parseJSON _ = mzero

instance ToSchema BorrowRequest where declareNamedSchema = sopDeclareNamedSchema
