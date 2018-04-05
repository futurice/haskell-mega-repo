{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.Constants.Internal (module Futurice.Constants.Internal) where

import Data.Aeson.Compat (FromJSON (..), withObject, (.:))
import Futurice.Prelude
import Prelude ()

data Constants = Constants
    { avatarPublicUrl   :: Text
    , fumPublicUrl      :: Text
    , personioPublicUrl :: Text
    , planmillPublicUrl :: Text
    , supportEmailAddr  :: Text
    , competenceMap     :: Map Text Text
    }
  deriving (Eq, Show)

instance FromJSON Constants where
    parseJSON = withObject "Constants" $ \obj -> Constants
        <$> obj .: "avatarPublicUrl"
        <*> obj .: "fumPublicUrl"
        <*> obj .: "personioPublicUrl"
        <*> obj .: "planmillPublicUrl"
        <*> obj .: "supportEmail"
        <*> obj .: "competenceMap"

-- TODO: remove when we drop support for GHC-7.10
deriveLift ''Constants
