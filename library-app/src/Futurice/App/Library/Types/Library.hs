{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Library.Types.Library where

import Data.Aeson
import Data.Swagger
import Database.PostgreSQL.Simple.FromField

import qualified Data.ByteString.Char8 as C
import qualified Data.Text             as T

import Futurice.Office
import Futurice.Prelude
import Prelude ()

data Library
    = OfficeLibrary Office
    | Elibrary
    | UnknownLibrary
    deriving (Eq, Show, Ord, Typeable, Generic)

deriveGeneric ''Library

instance FromField Library where
    fromField _ mdata = return library
        where library =
                  let officeText = T.pack <$> C.unpack <$> mdata >>= officeFromText
                  in case officeText of
                      Just office -> OfficeLibrary office
                      Nothing -> case mdata of
                          Just "Elibrary" -> Elibrary
                          _ -> UnknownLibrary

instance ToSchema Library where
    declareNamedSchema _ = do
        return $ NamedSchema (Just "Library") $ mempty
            & type_ .~ SwaggerObject
            & required .~ [ "office" ]

instance ToJSON Library where
    toJSON (OfficeLibrary office) = object ["office" .= office]
    toJSON Elibrary               = object ["office" .= ("Elibrary" :: Text)]
    toJSON _                      = object ["office" .= ("Unknown" :: Text)]

instance FromJSON Library where
    parseJSON = withObject "Library" $ \l -> do
        office <- l .: "office"
        case officeFromText office of
            Just o -> pure $ OfficeLibrary o
            Nothing -> if office == "Elibrary" then pure Elibrary else pure UnknownLibrary
