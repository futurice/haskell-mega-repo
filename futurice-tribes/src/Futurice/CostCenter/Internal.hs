{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.CostCenter.Internal (module Futurice.CostCenter.Internal) where

import Data.Aeson.Compat          (FromJSON (..), withText)
import Data.Char                  (isDigit)
import Futurice.Prelude
import Language.Haskell.TH.Syntax (Lift)
import Prelude ()

import qualified Text.Trifecta as T

data CostCenterInfo = CostCenterInfo
    { cciName :: !Text
    , cciCode :: !Word
    }
  deriving (Eq, Show, Lift)

codeP :: T.TokenParsing m => m Word
codeP = read <$> some T.digit <* T.satisfy (not . isDigit)

parseCostCenterCode :: Text -> Either String Word
parseCostCenterCode t = case T.parseString codeP mempty (t ^. unpacked) of
    T.Success x -> Right x
    T.Failure e -> Left (show (T._errDoc e))

instance FromJSON CostCenterInfo where
    parseJSON = withText "CostCenterInfo" $ \t -> do
        code <- either fail pure $ parseCostCenterCode t
        pure (CostCenterInfo t code)
