{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Library.Types.BookInformationByISBNResponse where

import Data.Aeson
import Futurice.App.Sisosota.Types (ContentHash)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.Types.BookInformation
import Futurice.App.Library.Types.Library

data DataSource = DSDatabase
                | DSAmazon
                deriving (Eq, Ord, Show, Generic, ToJSON, ToSchema)

data BookInformationByISBNResponse = BookInformationByISBNResponse
    { _byISBNId              :: !BookInformationId
    , _byISBNTitle           :: !Text
    , _byISBNISBN            :: !Text
    , _byISBNAuthor          :: !Text
    , _byISBNPublisher       :: !Text
    , _byISBNPublished       :: !Int
    , _byISBNCover           :: !ContentHash
    , _byISBNAmazonLink      :: !Text
    , _byISBNBooks           :: !(Map Library Int)
    , _byISBNDataSource      :: !DataSource
    } deriving  (Show, Typeable, Generic)

deriveGeneric ''BookInformationByISBNResponse

deriveVia [t| ToJSON BookInformationByISBNResponse `Via` Sopica BookInformationByISBNResponse |]

instance ToSchema BookInformationByISBNResponse
