{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric         #-}
module Personio.EmployeeStatus (
    Status(..)) where

import Data.Aeson.Compat (Value (String), withText)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()
import Data.Swagger               (NamedSchema (..))

import qualified Data.Map  as Map
import qualified Data.Text as T

-- |Employee contractual status with initial state being Onboarding
-- Personio changes status to Inactive on reaching Employee.endDate
data Status
    = Active
    | Inactive
    | Onboarding
    | Leave
    deriving (Eq, Ord, Show, Read, Typeable, Enum, Bounded, Generic)

makePrisms ''Status
deriveGeneric ''Status

instance NFData Status

instance Arbitrary Status where
    arbitrary = sopArbitrary
    shrink    = sopShrink

statusToText :: Status -> Text
statusToText Active = "active"
statusToText Inactive = "inactive"
statusToText Onboarding = "onboarding"
statusToText Leave = "leave"

instance ToSchema (Status) where
    declareNamedSchema _ = pure $ NamedSchema (Just "Status") mempty

statusFromText :: Text -> Maybe Status
statusFromText t = Map.lookup (T.toLower t) m
  where
    m = Map.fromList $ map (\x -> (T.toLower $ statusToText x, x)) [minBound .. maxBound]

_Status :: Prism' Text Status
_Status = prism' statusToText statusFromText

instance ToJSON Status where
    toJSON = String . statusToText

instance FromJSON Status where
    parseJSON = withText "Status" $ \t ->
        maybe (fail $ "invalid Status " <> t ^. unpacked) pure $ t ^? _Status
