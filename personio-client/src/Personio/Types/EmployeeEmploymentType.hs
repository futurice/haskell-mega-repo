{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Personio.Types.EmployeeEmploymentType
    (EmploymentType (..), employmentTypeFromText
    ) where

import Data.Aeson.Compat (Value (String), withText)
import Data.Swagger (NamedSchema (..))
import Futurice.Generics
import Futurice.Prelude

import qualified Data.Map as Map
import qualified Data.Text as T

data EmploymentType
    = Internal
    | External
    deriving (Eq, Ord, Show, Read, Typeable, Enum, Bounded, Generic)

makePrisms ''EmploymentType
deriveGeneric ''EmploymentType

instance NFData EmploymentType

instance Arbitrary EmploymentType where
    arbitrary = sopArbitrary
    shrink    = sopShrink

employmentTypeToText :: EmploymentType -> Text
employmentTypeToText Internal = "internal"
employmentTypeToText External = "external"

instance ToSchema EmploymentType where
    declareNamedSchema _ = pure $ NamedSchema (Just "Employment type") mempty

employmentTypeFromText :: Text -> Maybe EmploymentType
employmentTypeFromText t = Map.lookup (T.toLower t) m
  where
    m = Map.fromList $
        map (\x -> (T.toLower $ employmentTypeToText x, x))
        [minBound .. maxBound]

_EmploymentType :: Prism' Text EmploymentType
_EmploymentType = prism' employmentTypeToText employmentTypeFromText

instance ToJSON EmploymentType where
    toJSON = String . employmentTypeToText

instance FromJSON EmploymentType where
    parseJSON = withText "Employment type" $ \t ->
        maybe (fail $ "invalid Employment type " <> t ^. unpacked) pure
        $ t ^? _EmploymentType
