{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.Office.Internal (module Futurice.Office.Internal) where

import Data.Aeson.Compat
       (FromJSON (..), withObject, (.!=), (.:), (.:?))
import Futurice.Prelude
import Futurice.Company
import Language.Haskell.TH.Syntax (Lift)
import Prelude ()

data OfficeInfo = OfficeInfo
    { offName        :: !Text
    , offShortName   :: !Text
    , offCompany     :: !Company
    , offDefault     :: !Bool
    }
  deriving (Eq, Show, Lift)

instance FromJSON OfficeInfo where
    parseJSON = withObject "OfficeInfo" $ \obj -> OfficeInfo
        <$> obj .: "name"
        <*> obj .: "short"
        <*> obj .: "company"
        <*> obj .:? "default" .!= False
