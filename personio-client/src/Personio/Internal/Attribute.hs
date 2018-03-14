{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Personio.Internal.Attribute where

import Data.Aeson.Compat
import Futurice.Aeson
import Futurice.Generics
import Data.Aeson.Internal         (JSONPathElement (Key), (<?>))
import Futurice.Prelude
import Prelude ()

import qualified Data.HashMap.Strict as HM
import qualified Data.Swagger        as Swagger
import qualified Data.Text           as T

-- | Personio attribute, i.e. labeled value.
data Attribute = Attribute !Text !Value deriving (Eq, Show, Generic)

instance ToJSON Attribute where
    toJSON (Attribute l v) = object [ "label" .= l, "value" .= v ]

instance FromJSON Attribute where
    parseJSON = withObjectDump "Attribute" $ \obj -> Attribute
        <$> obj .: "label"
        <*> obj .: "value"

instance ToSchema Attribute where
    declareNamedSchema _ = pure $ Swagger.NamedSchema (Just "Attribute") mempty

instance NFData Attribute

instance Arbitrary Attribute where
    arbitrary = pure (Attribute "arbitrary" "value")

-------------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------------

data Attributes = Attributes
    { attrCore    :: HashMap Text Value
    , attrDynamic :: HashMap Text Value
    }
  deriving Show

instance FromJSON Attributes where
    parseJSON j = mkAttributes <$> parseJSON j

mkAttributes :: HashMap Text Attribute -> Attributes
mkAttributes m = Attributes (mapHM toCore m) (mapHM toDynamic m)
  where
    toCore k (Attribute _ v)
        | "dynamic_" `T.isPrefixOf` k = Nothing
        | otherwise                   = Just (k, v)

    toDynamic k (Attribute l v)
        | "dynamic_" `T.isPrefixOf` k = Just (l, v)
        | otherwise                   = Nothing

    mapHM
        :: (Eq k2, Hashable k2)
        => (k1 -> v1 -> Maybe (k2, v2))
        -> HashMap k1 v1 -> HashMap k2 v2
    mapHM f = HM.fromList . mapMaybe (uncurry f) . HM.toList

parseAttribute :: FromJSON a => Attributes -> Text -> Parser a
parseAttribute (Attributes obj _) attrName = case HM.lookup attrName obj of
    Nothing -> fail $ "core attribute " ++ show attrName ++ " not present"
    Just v  -> parseJSON v <?> Key attrName

parseDynamicAttribute :: FromJSON a => Attributes -> Text -> Parser a
parseDynamicAttribute (Attributes _ obj) attrName = case HM.lookup attrName obj of
    Nothing -> fail $ "dynamic attribute " ++ show attrName ++ " not present"
    Just v  -> parseJSON v <?> Key attrName
