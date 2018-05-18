{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.Company.Internal (module Futurice.Company.Internal) where

import Data.Aeson.Compat
       (FromJSON (..), withObject, (.:))
import Futurice.Prelude
import Language.Haskell.TH.Syntax (Lift)
import Prelude ()

data CompanyInfo = CompanyInfo
    { cName        :: !Text
    , cCountry     :: !Text
    }
  deriving (Eq, Show, Lift)

instance FromJSON CompanyInfo where
    parseJSON = withObject "CompanyInfo" $ \obj -> CompanyInfo
        <$> obj .: "name"
        <*> obj .: "country"
