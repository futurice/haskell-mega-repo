{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Futurice.Office (
    Office,
    mkOffice,
    defaultOffice,
    officeToText,
    officeFromText,
    officeShortName,
    officeCompany,
    -- * Prism
    _Office,
    -- * known values
    offHelsinki,
    offTampere,
    offOther,
    ) where

import Control.Monad           ((>=>))
import Futurice.Generics
import Futurice.Prelude
import Futurice.Company
import Futurice.Office.Internal
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

-- | Office.
newtype Office = Office Int
  deriving (Eq, Ord)

deriveLift ''Office

instance Show Office where
    showsPrec d t = showsPrec d (officeToText t)

instance Enum Office where
    fromEnum (Office i) = i
    toEnum i
        | 0 <= i && i < V.length officeInfos = Office i
        | otherwise = error "toEnum @Office out of bounds"

instance Bounded Office where
    minBound = Office 0 -- assume there is at least one office!
    maxBound = Office (V.length officeInfos - 1)

instance Hashable Office where
    hashWithSalt salt (Office i) = hashWithSalt salt i

-------------------------------------------------------------------------------
-- Values
-------------------------------------------------------------------------------

offHelsinki :: Office
offHelsinki = fromMaybe (error "Office Helsinki") $ officeFromText "Helsinki"

offTampere :: Office
offTampere = fromMaybe (error "Office Tampere") $ officeFromText "Tampere"

offOther :: Office
offOther = fromMaybe (error "Office Other") $ officeFromText "Other"

-------------------------------------------------------------------------------
-- Magic
-------------------------------------------------------------------------------

-- | Vector to have constant tifme lookups.
officeInfos :: Vector OfficeInfo
officeInfos
    = V.fromList
    $ $(makeRelativeToProject "offices.json" >>= embedFromJSON (Proxy :: Proxy [OfficeInfo]))

-- | Map from lowercased office names and aliases to index in officeInfos and OfficeInfo itself
officeLookup :: Map Text (Int, OfficeInfo)
officeLookup
    = Map.fromList
    $ concatMap f
    $ zip [0..]
    $ toList officeInfos
  where
    f (i, off) = [ (T.toLower (offName off), (i, off)) ]

-------------------------------------------------------------------------------
-- Default
-------------------------------------------------------------------------------

-- | Some office with @default: true@, or  an 'error' if non found.
defaultOffice :: Office
defaultOffice
    = Office
    $ fromMaybe (error "defaultOffice: No default Office")
    $ V.findIndex offDefault officeInfos

-------------------------------------------------------------------------------
-- Template Haskell
-------------------------------------------------------------------------------

-- | create office compile time.
--
-- /Note:/ use only in tests, do not hardcode offices!
mkOffice :: String -> ExpQ
mkOffice n
    | Just t <- officeFromText (n ^. packed) = [| t |]
    | otherwise = fail $ "Invalid office name: " ++ n

-------------------------------------------------------------------------------
-- Funcoffons
-------------------------------------------------------------------------------

officeInfo :: Office -> OfficeInfo
officeInfo (Office i)
    = fromMaybe (error "officeInfo: invalid Office")
    $ officeInfos ^? ix i

officeName :: Office -> Text
officeName = offName . officeInfo

officeShortName :: Office -> Text
officeShortName = offShortName . officeInfo

officeCompany :: Office -> Company
officeCompany = offCompany . officeInfo

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

officeToText :: Office -> Text
officeToText = officeName

officeFromText :: Text -> Maybe Office
officeFromText k = Office <$> officeLookup ^? ix (T.toLower k) . _1

officeFromTextE :: Text -> Either String Office
officeFromTextE k =
    maybe (Left $ "Invalid office " ++ show k) Right (officeFromText k)

_Office :: Prism' Text Office
_Office = prism' officeToText officeFromText

instance NFData Office where
    rnf (Office i) = rnf i

instance Binary Office where
    put (Office i) = put i
    get = Office <$> get

instance Arbitrary Office where
    arbitrary = QC.elements [ Office i | i <- [0 .. V.length officeInfos - 1] ]

instance ToHtml Office where
    toHtmlRaw = toHtml
    toHtml = toHtml . officeToText

instance ToParamSchema Office where
    toParamSchema _ = mempty
        & Swagger.type_ .~ Swagger.SwaggerString
        -- & Swagger.enum_ ?~ map enumToJSON_ enumUniverse_

instance ToSchema Office where
    declareNamedSchema p = pure $ Swagger.NamedSchema (Just "Office") $ mempty
        & Swagger.paramSchema .~ toParamSchema p

instance ToJSON Office where
    toJSON = Aeson.String . officeToText

instance FromJSON Office where
    parseJSON = Aeson.withTextDump "Office" $
        either (fail . view unpacked) pure . officeFromTextE

instance Csv.ToField Office where
    toField = Csv.toField . officeToText

instance Csv.FromField Office where
    parseField = Csv.parseField >=>
        either (fail . view unpacked) pure . officeFromTextE

instance FromHttpApiData Office where
    parseUrlPiece = first (view packed) . officeFromTextE

instance ToHttpApiData Office where
    toUrlPiece = officeToText
