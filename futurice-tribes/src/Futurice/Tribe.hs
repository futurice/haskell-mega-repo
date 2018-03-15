{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.Tribe (
    Tribe,
    mkTribe,
    tribeToText,
    tribeFromText,
    _Tribe,
    tribeOffices,
    tribeCostCenters,
    defaultTribe,
    ) where

import Control.Monad           ((>=>))
import Futurice.CostCenter
import Futurice.Generics
import Futurice.Office
import Futurice.Prelude
import Futurice.Tribe.Internal
import Language.Haskell.TH     (ExpQ)
import Lucid                   (ToHtml (..))
import Prelude ()

import qualified Data.Csv        as Csv
import qualified Data.Map        as Map
import qualified Data.Swagger    as Swagger
import qualified Data.Text       as T
import qualified Data.Vector     as V
import qualified Futurice.Aeson  as Aeson
import qualified Test.QuickCheck as QC

-- | Tribe.
newtype Tribe = Tribe Int
  deriving (Eq, Ord)

deriveLift ''Tribe

instance Show Tribe where
    showsPrec d t = showsPrec d (tribeToText t)

instance Enum Tribe where
    fromEnum (Tribe i) = i
    toEnum i
        | 0 <= i && i < V.length tribeInfos = Tribe i
        | otherwise = error "toEnum @Tribe out of bounds"

instance Bounded Tribe where
    minBound = Tribe 0 -- assume there is at least one tribe!
    maxBound = Tribe (V.length tribeInfos - 1)

instance Hashable Tribe where
    hashWithSalt salt (Tribe i) = hashWithSalt salt i

-------------------------------------------------------------------------------
-- Magic
-------------------------------------------------------------------------------

-- | Vector to have constant time lookups.
tribeInfos :: Vector TribeInfo
tribeInfos
    = V.fromList
    $ $(makeRelativeToProject "tribes.json" >>= embedFromJSON (Proxy :: Proxy [TribeInfo]))

-- | Map from lowercased tribe names and aliases to index in tribeInfos and TribeInfo itself
tribeLookup :: Map Text (Int, TribeInfo)
tribeLookup
    = Map.fromList
    $ concatMap f
    $ zip [0..]
    $ toList tribeInfos
  where
    f (i, ti) = [ (T.toLower k, (i, ti)) | k <- tiName ti : tiAliases ti ]

-------------------------------------------------------------------------------
-- Default
-------------------------------------------------------------------------------

-- | Some tribe with @default: true@, or  an 'error' if non found.
defaultTribe :: Tribe
defaultTribe
    = Tribe
    $ fromMaybe (error "defaultTribe: No default Tribe")
    $ V.findIndex tiDefault tribeInfos

-------------------------------------------------------------------------------
-- Template Haskell
-------------------------------------------------------------------------------

-- | create tribe compile time.
--
-- /Note:/ use only in tests, do not hardcode tribes!
mkTribe :: String -> ExpQ
mkTribe n
    | Just t <- tribeFromText (n ^. packed) = [| t |]
    | otherwise = fail $ "Invalid tribe name: " ++ n

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

tribeInfo :: Tribe -> TribeInfo
tribeInfo (Tribe i)
    = fromMaybe (error "tribeInfo: invalid Tribe")
    $ tribeInfos ^? ix i

tribeName :: Tribe -> Text
tribeName = tiName . tribeInfo

tribeOffices :: Tribe -> [Office]
tribeOffices = tiOffices . tribeInfo

tribeCostCenters :: Tribe -> NonEmpty CostCenter
tribeCostCenters = tiCostCenters . tribeInfo

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

tribeToText :: Tribe -> Text
tribeToText = tribeName

tribeFromText :: Text -> Maybe Tribe
tribeFromText k = Tribe <$> tribeLookup ^? ix (T.toLower k) . _1

tribeFromTextE :: Text -> Either String Tribe
tribeFromTextE k =
    maybe (Left $ "Invalid tribe " ++ show k) Right (tribeFromText k)

_Tribe :: Prism' Text Tribe
_Tribe = prism' tribeToText tribeFromText

instance NFData Tribe where
    rnf (Tribe i) = rnf i

instance Binary Tribe where
    put (Tribe i) = put i
    get = Tribe <$> get

instance Arbitrary Tribe where
    arbitrary = QC.elements [ Tribe i | i <- [0 .. V.length tribeInfos - 1] ]

instance ToHtml Tribe where
    toHtmlRaw = toHtml
    toHtml = toHtml . tribeToText

instance ToParamSchema Tribe where
    toParamSchema _ = mempty
        & Swagger.type_ .~ Swagger.SwaggerString
        -- & Swagger.enum_ ?~ map enumToJSON_ enumUniverse_

instance ToSchema Tribe where
    declareNamedSchema p = pure $ Swagger.NamedSchema (Just "Tribe") $ mempty
        & Swagger.paramSchema .~ toParamSchema p

instance ToJSON Tribe where
    toJSON = Aeson.String . tribeToText

instance FromJSON Tribe where
    parseJSON = Aeson.withTextDump "Tribe" $
        either (fail . view unpacked) pure . tribeFromTextE

instance Csv.ToField Tribe where
    toField = Csv.toField . tribeToText

instance Csv.FromField Tribe where
    parseField = Csv.parseField >=>
        either (fail . view unpacked) pure . tribeFromTextE

instance FromHttpApiData Tribe where
    parseUrlPiece = first (view packed) . tribeFromTextE

instance ToHttpApiData Tribe where
    toUrlPiece = tribeToText
