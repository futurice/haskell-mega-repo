{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.CareerLevel (
    CareerLevel,
    mkCareerLevel,
    careerLevelToText,
    careerLevelFromText,
    _CareerLevel,
    defaultCareerLevel,
    ) where

import Control.Monad                 ((>=>))
import Data.Aeson.Types
       (FromJSONKey (..), FromJSONKeyFunction (..), ToJSONKey (..),
       toJSONKeyText)
import Futurice.CareerLevel.Internal
import Futurice.Generics
import Futurice.Prelude
import Language.Haskell.TH           (ExpQ)
import Lucid                         (ToHtml (..))
import Prelude ()

import qualified Data.Csv            as Csv
import qualified Data.Map            as Map
import qualified Data.Swagger        as Swagger
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Futurice.Aeson      as Aeson
import qualified Futurice.Chart.Enum as C
import qualified Test.QuickCheck     as QC

-- | CareerLevel.
newtype CareerLevel = CareerLevel Int
  deriving (Eq, Ord, Lift)

instance Show CareerLevel where
    showsPrec d t = showsPrec d (careerLevelToText t)

instance Enum CareerLevel where
    fromEnum (CareerLevel i) = i
    toEnum i
        | 0 <= i && i < V.length careerLevelInfos = CareerLevel i
        | otherwise = error "toEnum @CareerLevel out of bounds"

instance Bounded CareerLevel where
    minBound = CareerLevel 0 -- assume there is at least one careerLevel!
    maxBound = CareerLevel (V.length careerLevelInfos - 1)

instance Hashable CareerLevel where
    hashWithSalt salt (CareerLevel i) = hashWithSalt salt i

instance C.PlotValue CareerLevel where
    toValue   = C.enumToValue
    fromValue = C.enumFromValue
    autoAxis  = C.enumAutoAxis (T.take 6 . careerLevelName)

instance IsString CareerLevel where
    fromString = careerLevelFromTextDef . fromString

-------------------------------------------------------------------------------
-- Magic
-------------------------------------------------------------------------------

-- | Vector to have constant time lookups.
careerLevelInfos :: Vector CareerLevelInfo
careerLevelInfos
    = V.fromList . toList
    $ $(makeRelativeToProject "career-levels.json" >>= embedFromJSON (Proxy :: Proxy (NonEmpty CareerLevelInfo)))

-- | Map from lowercased careerLevel names and aliases to index in careerLevelInfos and CareerLevelInfo itself
careerLevelLookup :: Map Text (Int, CareerLevelInfo)
careerLevelLookup
    = Map.fromList
    $ map f
    $ zip [0..]
    $ toList careerLevelInfos
  where
    f (i, ti) = (cliName ti, (i, ti))

-------------------------------------------------------------------------------
-- Default
-------------------------------------------------------------------------------

-- | Some careerLevel with @default: true@, or  an 'error' if non found.
defaultCareerLevel :: CareerLevel
defaultCareerLevel
    = CareerLevel
    $ V.length careerLevelInfos - 1

-------------------------------------------------------------------------------
-- Template Haskell
-------------------------------------------------------------------------------

-- | create careerLevel compile time.
--
-- /Note:/ use only in tests, do not hardcode careerLevels!
mkCareerLevel :: String -> ExpQ
mkCareerLevel n
    | Just t <- careerLevelFromText (n ^. packed) = [| t |]
    | otherwise = fail $ "Invalid careerLevel name: " ++ n

-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

careerLevelInfo :: CareerLevel -> CareerLevelInfo
careerLevelInfo (CareerLevel i)
    = fromMaybe (error "careerLevelInfo: invalid CareerLevel")
    $ careerLevelInfos ^? ix i

careerLevelName :: CareerLevel -> Text
careerLevelName = cliName . careerLevelInfo

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

careerLevelToText :: CareerLevel -> Text
careerLevelToText = careerLevelName

careerLevelFromText :: Text -> Maybe CareerLevel
careerLevelFromText k = CareerLevel <$> careerLevelLookup ^? ix (T.toLower k) . _1

careerLevelFromTextDef :: Text -> CareerLevel
careerLevelFromTextDef t = fromMaybe defaultCareerLevel $ careerLevelFromText t

careerLevelFromTextE :: Text -> Either String CareerLevel
careerLevelFromTextE k =
    maybe (Left $ "Invalid career level " ++ show k) Right (careerLevelFromText k)

_CareerLevel :: Prism' Text CareerLevel
_CareerLevel = prism' careerLevelToText careerLevelFromText

instance NFData CareerLevel where
    rnf (CareerLevel i) = rnf i

instance Binary CareerLevel where
    put (CareerLevel i) = put i
    get = CareerLevel <$> get

instance Arbitrary CareerLevel where
    arbitrary = QC.elements [ CareerLevel i | i <- [0 .. V.length careerLevelInfos - 1] ]

instance ToHtml CareerLevel where
    toHtmlRaw = toHtml
    toHtml = toHtml . careerLevelToText

instance ToParamSchema CareerLevel where
    toParamSchema _ = mempty
        & Swagger.type_ .~ Just Swagger.SwaggerString
        -- & Swagger.enum_ ?~ map enumToJSON_ enumUniverse_

instance ToSchema CareerLevel where
    declareNamedSchema p = pure $ Swagger.NamedSchema (Just "CareerLevel") $ mempty
        & Swagger.paramSchema .~ toParamSchema p

instance ToJSON CareerLevel where
    toJSON = Aeson.String . careerLevelToText

instance FromJSON CareerLevel where
    parseJSON = Aeson.withTextDump "CareerLevel" $
         pure . careerLevelFromTextDef

instance ToJSONKey CareerLevel where
    toJSONKey = toJSONKeyText careerLevelToText

instance FromJSONKey CareerLevel where
    fromJSONKey = FromJSONKeyTextParser $
        either (fail . view unpacked) pure . careerLevelFromTextE

instance Csv.ToField CareerLevel where
    toField = Csv.toField . careerLevelToText

instance Csv.FromField CareerLevel where
    parseField = Csv.parseField >=>
        either (fail . view unpacked) pure . careerLevelFromTextE

instance FromHttpApiData CareerLevel where
    parseUrlPiece = first (view packed) . careerLevelFromTextE

instance ToHttpApiData CareerLevel where
    toUrlPiece = careerLevelToText
