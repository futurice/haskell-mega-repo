{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
-- TODO: split into submodules
module Power.Types where

import Data.Aeson        (withObject, (.:))
import FUM.Types.Login   (Login)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Power.PyJSON

-------------------------------------------------------------------------------
-- /people
-------------------------------------------------------------------------------

data Person = Person
    { personLogin     :: Login  -- ^ TODO: remove when Power will have Personio ID
    , personUtzTarget :: Int    -- ^ integer percentages
    }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData)

deriveGeneric ''Person

deriveVia [t| ToJSON Person   `Via` Sopica Person |]
deriveVia [t| FromJSON Person `Via` Sopica Person |]

instance PyJSON Person where
    parsePyJSON = withObject "Person" $ \obj -> Person
        <$> obj .: "username"
        <*> obj .: "utz_target"
