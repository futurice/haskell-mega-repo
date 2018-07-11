{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Library.Types.Library where

import Data.Swagger
       (defaultSchemaOptions, genericDeclareNamedSchemaUnrestricted)
import Database.PostgreSQL.Simple.FromField

import qualified Data.ByteString.Char8 as C
import qualified Data.Text             as T

import Futurice.Generics
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

instance ToSchema Library where declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions
instance ToJSON Library
instance FromJSON Library
