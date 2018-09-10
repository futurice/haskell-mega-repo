{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Library.Types.BookInformationByISBNResponse where

import Data.Aeson
import Data.Swagger
import Futurice.App.Sisosota.Types (ContentHash)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.Types.BookInformation
import Futurice.App.Library.Types.Library

data DataSource = DSDatabase BookInformationId ContentHash
                | DSAmazon Text
                deriving (Show, Generic)

instance ToSchema DataSource where
    declareNamedSchema _ = do
        return $ NamedSchema (Just "Datasource") $ mempty
            & type_ .~ SwaggerObject
            & required .~ [ ]
            & type_ .~ SwaggerObject

instance ToJSON DataSource where
    toJSON (DSDatabase binfoId contentHash) = object ["source" .= ("Database" :: Text), "bookinformationid" .= binfoId, "coverhash" .= contentHash]
    toJSON (DSAmazon imageUrl)              = object ["source" .= ("Amazon" :: Text), "coverurl" .= imageUrl]

data BookInformationByISBNResponse = BookInformationByISBNResponse
    { _byISBNTitle           :: !Text
    , _byISBNISBN            :: !Text
    , _byISBNAuthor          :: !Text
    , _byISBNPublisher       :: !Text
    , _byISBNPublished       :: !Int
    , _byISBNAmazonLink      :: !Text
    , _byISBNBooks           :: !(Map Library Int)
    , _byISBNDataSource      :: !DataSource
    } deriving (Show, Typeable, Generic, ToSchema)

deriveGeneric ''BookInformationByISBNResponse

deriveVia [t| ToJSON BookInformationByISBNResponse `Via` Sopica BookInformationByISBNResponse |]
