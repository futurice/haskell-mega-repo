{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.CostCenter (
    CostCenter,
    mkCostCenter,
    costCenterToText,
    costCenterFromText,
    _CostCenter,
    ) where

import Control.Monad                ((>=>))
import Futurice.CostCenter.Internal
import Futurice.Generics
import Futurice.Prelude
import Language.Haskell.TH          (ExpQ)
import Lucid                        (ToHtml (..))
import Prelude ()

import qualified Data.Csv        as Csv
import qualified Data.Map        as Map
import qualified Data.Swagger    as Swagger
import qualified Data.Vector     as V
import qualified Futurice.Aeson  as Aeson
import qualified Test.QuickCheck as QC


-- | CostCenter is not a Tribe.
newtype CostCenter = CostCenter Int
  deriving (Eq, Ord)

deriveLift ''CostCenter

instance Show CostCenter where
    showsPrec d t = showsPrec d (costCenterToText t)

instance Enum CostCenter where
    fromEnum (CostCenter i) = i
    toEnum i
        | 0 <= i && i < V.length costCenterInfos = CostCenter i
        | otherwise = error "toEnum @CostCenter out of bounds"

instance Bounded CostCenter where
    minBound = CostCenter 0 -- assume there is at least one costCenter!
    maxBound = CostCenter (V.length costCenterInfos - 1)

instance Hashable CostCenter where
    hashWithSalt salt (CostCenter i) = hashWithSalt salt i

-------------------------------------------------------------------------------
-- Magic
-------------------------------------------------------------------------------

-- | Vector to have constant time lookups.
costCenterInfos :: Vector CostCenterInfo
costCenterInfos
    = V.fromList
    $ $(makeRelativeToProject "cost-centers.json" >>= embedFromJSON (Proxy :: Proxy [CostCenterInfo]))

-- | Map from lowercased costCenter names and aliases to index in costCenterInfos and CostCenterInfo itself
costCenterLookup :: Map Word (Int, CostCenterInfo)
costCenterLookup
    = Map.fromList
    $ map f
    $ zip [0..]
    $ toList costCenterInfos
  where
    f (i, cci) = (cciCode cci, (i, cci))

-------------------------------------------------------------------------------
-- Template Haskell
-------------------------------------------------------------------------------

-- | create costCenter compile time.
--
-- /Note:/ use only in tests, do not hardcode costCenters!
mkCostCenter :: String -> ExpQ
mkCostCenter n
    | Just t <- costCenterFromText (n ^. packed) = [| t |]
    | otherwise = fail $ "Invalid costCenter name: " ++ n

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

costCenterInfo :: CostCenter -> CostCenterInfo
costCenterInfo (CostCenter i)
    = fromMaybe (error "costCenterInfo: invalid CostCenter")
    $ costCenterInfos ^? ix i

costCenterName :: CostCenter -> Text
costCenterName = cciName . costCenterInfo

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

costCenterToText :: CostCenter -> Text
costCenterToText = costCenterName

-- | Note: only the leading number is considered.
costCenterFromText :: Text -> Maybe CostCenter
costCenterFromText k = do
    code <- either (const Nothing) Just $ parseCostCenterCode k
    CostCenter <$> costCenterLookup ^? ix code . _1

costCenterFromTextE :: Text -> Either String CostCenter
costCenterFromTextE k =
    maybe (Left $ "Invalid costCenter " ++ show k) Right (costCenterFromText k)

_CostCenter :: Prism' Text CostCenter
_CostCenter = prism' costCenterToText costCenterFromText

instance NFData CostCenter where
    rnf (CostCenter i) = rnf i

instance Binary CostCenter where
    put (CostCenter i) = put i
    get = CostCenter <$> get

instance Arbitrary CostCenter where
    arbitrary = QC.elements [ CostCenter i | i <- [0 .. V.length costCenterInfos - 1] ]

instance ToHtml CostCenter where
    toHtmlRaw = toHtml
    toHtml = toHtml . costCenterToText

instance ToParamSchema CostCenter where
    toParamSchema _ = mempty
        & Swagger.type_ .~ Swagger.SwaggerString
        -- & Swagger.enum_ ?~ map enumToJSON_ enumUniverse_

instance ToSchema CostCenter where
    declareNamedSchema p = pure $ Swagger.NamedSchema (Just "CostCenter") $ mempty
        & Swagger.paramSchema .~ toParamSchema p

instance ToJSON CostCenter where
    toJSON = Aeson.String . costCenterToText

instance FromJSON CostCenter where
    parseJSON = Aeson.withTextDump "CostCenter" $
        either (fail . view unpacked) pure . costCenterFromTextE

instance Csv.ToField CostCenter where
    toField = Csv.toField . costCenterToText

instance Csv.FromField CostCenter where
    parseField = Csv.parseField >=>
        either (fail . view unpacked) pure . costCenterFromTextE

instance FromHttpApiData CostCenter where
    parseUrlPiece = first (view packed) . costCenterFromTextE

instance ToHttpApiData CostCenter where
    toUrlPiece = costCenterToText
