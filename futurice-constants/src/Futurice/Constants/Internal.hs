{-# LANGUAGE OverloadedStrings #-}
module Futurice.Constants.Internal (module Futurice.Constants.Internal) where

import Data.Aeson.Compat (FromJSON (..), withObject, (.:))
import Futurice.Prelude
import Prelude ()
import Servant.Client    (BaseUrl)

import Futurice.Services

data Constants = Constants
    { publicUrls       :: PerService Text
    , publicBaseUrls   :: PerService BaseUrl
    , supportEmailAddr :: Text
    , competenceMap    :: Map Text Text
    }
  deriving (Eq, Show, Lift)

instance FromJSON Constants where
    parseJSON = withObject "Constants" $ \obj -> Constants
        <$> obj .: "publicUrls"
        <*> obj .: "publicUrls"
        <*> obj .: "supportEmail"
        <*> obj .: "competenceMap"
