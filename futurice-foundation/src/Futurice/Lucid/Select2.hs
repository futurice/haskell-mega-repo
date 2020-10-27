{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.Lucid.Select2 (
    Select2Data (..),
    Select2Result (..),
    ) where

import Data.Aeson        (object, pairs, (.=))
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

-- | See <https://select2.org/data-sources/formats>.
data Select2Data = Select2Data
    { s2dId   :: !Text
    , s2dText :: !Text
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''Select2Data

deriveVia [t| ToJSON Select2Data   `Via` Sopica Select2Data |]
deriveVia [t| FromJSON Select2Data `Via` Sopica Select2Data |]

instance ToSchema Select2Data where declareNamedSchema = sopDeclareNamedSchema

-- | See <https://select2.org/data-sources/formats>.
newtype Select2Result = Select2Result [Select2Data]
  deriving (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''Select2Result

instance ToJSON Select2Result where
    toJSON (Select2Result xs) = object [ "results" .= xs ]
    toEncoding (Select2Result xs) = pairs $ "results" .= xs

-- | TODO: incorrect
instance ToSchema Select2Result where declareNamedSchema = emptyDeclareNamedSchema
