{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.CareerLevel.Internal (module Futurice.CareerLevel.Internal) where

import Data.Aeson.Compat          (FromJSON (..))
import Futurice.Prelude
import Language.Haskell.TH.Syntax (Lift)
import Prelude ()

newtype CareerLevelInfo = CareerLevelInfo { cliName :: Text }
  deriving Lift

instance FromJSON CareerLevelInfo where
    parseJSON = fmap CareerLevelInfo . parseJSON
