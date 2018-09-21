{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.Tribe.Internal (module Futurice.Tribe.Internal) where

import Data.Aeson.Compat
       (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Futurice.CostCenter        (CostCenter)
import Futurice.Office            (Office)
import Futurice.Prelude
import Language.Haskell.TH.Syntax (Lift)
import Prelude ()

data TribeInfo = TribeInfo
    { tiName        :: !Text
    , tiOffices     :: [Office]
    , tiAliases     :: [Text]
    , tiCostCenters :: NonEmpty CostCenter
    , tiDefault     :: !Bool
    -- tiSubsidiary :: !Bool -- TODO: ?
    }
  deriving (Eq, Show, Lift)

instance FromJSON TribeInfo where
    parseJSON = withObject "TribeInfo" $ \obj -> TribeInfo
        <$> obj .: "name"
        <*> obj .:? "offices" .!= []
        <*> obj .:? "aliases" .!= []
        <*> obj .: "costcenters"
        <*> obj .:? "default" .!= False
