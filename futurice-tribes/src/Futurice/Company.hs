{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Futurice.Company (
    -- * Company
    Company,
    mkCompany,
    companyToText,
    companyFromText,
    _Company,
    -- ** Known values
    companyFuturiceOy,
    companyFuturiceLtd,
    companyFuturiceGmbH,
    companyFutuSwedenAB,

    -- * Country
    Country (..),
    companyCountry,
    countryToText,
    countryToText',
    countryFromText,
    -- ** Known values
    countryFinland,
    ) where

import Control.Monad               ((>=>))
import Futurice.Company.Internal
import Futurice.Generics
import Futurice.Prelude
import Language.Haskell.TH         (ExpQ)
import Lucid                       (ToHtml (..))
import Prelude ()
import Text.Regex.Applicative.Text (few, match, psym, sym)

import qualified Data.Csv            as Csv
import qualified Data.Map            as Map
import qualified Data.Swagger        as Swagger
import qualified Data.Text           as T
import qualified Data.Vector         as V
import qualified Futurice.Aeson      as Aeson
import qualified Futurice.Chart.Enum as C
import qualified Test.QuickCheck     as QC

-- | Company.
newtype Company = Company Int
  deriving (Eq, Ord, Lift, Generic)

instance Show Company where
    showsPrec d t = showsPrec d (companyToText t)

instance Enum Company where
    fromEnum (Company i) = i
    toEnum i
        | 0 <= i && i < V.length companyInfos = Company i
        | otherwise = error "toEnum @Company out of bounds"

instance Bounded Company where
    minBound = Company 0 -- assume there is at least one company!
    maxBound = Company (V.length companyInfos - 1)

instance Hashable Company where
    hashWithSalt salt (Company i) = hashWithSalt salt i

instance C.PlotValue Company where
    toValue   = C.enumToValue
    fromValue = C.enumFromValue
    autoAxis  = C.enumAutoAxis companyName

-------------------------------------------------------------------------------
-- Values
-------------------------------------------------------------------------------

companyFuturiceOy :: Company
companyFuturiceOy =  fromMaybe (error "Company Futurice Oy") $ companyFromText "Futurice Oy"

companyFuturiceLtd :: Company
companyFuturiceLtd = fromMaybe (error "Company Futurice Ltd") $ companyFromText "Futurice Ltd"

companyFuturiceGmbH :: Company
companyFuturiceGmbH = fromMaybe (error "Company Futurice GmbH") $ companyFromText "Futurice GmbH"

companyFutuSwedenAB :: Company
companyFutuSwedenAB = fromMaybe (error "Company FutuSweden AB") $ companyFromText "Futu Sweden AB"

countryFinland :: Country
countryFinland = Country companyFuturiceOy

-------------------------------------------------------------------------------
-- Magic
-------------------------------------------------------------------------------

-- | Vector to have constant tifme lookups.
companyInfos :: Vector CompanyInfo
companyInfos
    = V.fromList
    $ $(makeRelativeToProject "companies.json" >>= embedFromJSON (Proxy :: Proxy [CompanyInfo]))

-- | Map from lowercased company names and aliases to index in companyInfos and CompanyInfo itself
companyLookup :: Map Text (Int, CompanyInfo)
companyLookup
    = Map.fromList
    $ concatMap f
    $ zip [0..]
    $ toList companyInfos
  where
    f (i, off) = [ (T.toLower (cName off), (i, off)) ]

countryLookup :: Map Text Country
countryLookup = Map.fromList
    [ (T.toLower (fromMaybe "No country" $ cCountry ci), Country (Company i))
    | (i, ci) <- Map.elems companyLookup
    ]

-------------------------------------------------------------------------------
-- Template Haskell
-------------------------------------------------------------------------------

-- | create company compile time.
--
-- /Note:/ use only in tests, do not hardcode companys!
mkCompany :: String -> ExpQ
mkCompany n
    | Just t <- companyFromText (n ^. packed) = [| t |]
    | otherwise = fail $ "Invalid company name: " ++ n

-------------------------------------------------------------------------------
-- Funcoffons
-------------------------------------------------------------------------------

companyInfo :: Company -> CompanyInfo
companyInfo (Company i)
    = fromMaybe (error "companyInfo: invalid Company")
    $ companyInfos ^? ix i

companyName :: Company -> Text
companyName = cName . companyInfo

companyCountry :: Company -> Country
companyCountry = Country

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

companyToText :: Company -> Text
companyToText = companyName

companyFromText :: Text -> Maybe Company
companyFromText k = Company <$> companyLookup ^? ix (T.toLower k) . _1

companyFromTextE :: Text -> Either String Company
companyFromTextE k =
    maybe (Left $ "Invalid company " ++ show k) Right (companyFromText k)

_Company :: Prism' Text Company
_Company = prism' companyToText companyFromText

instance AnsiPretty Company

instance NFData Company where
    rnf (Company i) = rnf i

instance Binary Company where
    put (Company i) = put i
    get = Company <$> get

instance Arbitrary Company where
    arbitrary = QC.elements [ Company i | i <- [0 .. V.length companyInfos - 1] ]

instance ToHtml Company where
    toHtmlRaw = toHtml
    toHtml = toHtml . companyToText

instance ToParamSchema Company where
    toParamSchema _ = mempty
        & Swagger.type_ .~ Swagger.SwaggerString
        -- & Swagger.enum_ ?~ map enumToJSON_ enumUniverse_

instance ToSchema Company where
    declareNamedSchema p = pure $ Swagger.NamedSchema (Just "Company") $ mempty
        & Swagger.paramSchema .~ toParamSchema p

instance ToJSON Company where
    toJSON = Aeson.String . companyToText

instance FromJSON Company where
    parseJSON = Aeson.withTextDump "Company" $
        either (fail . view unpacked) pure . companyFromTextE

instance Csv.ToField Company where
    toField = Csv.toField . companyToText

instance Csv.FromField Company where
    parseField = Csv.parseField >=>
        either (fail . view unpacked) pure . companyFromTextE

instance FromHttpApiData Company where
    parseUrlPiece = first (view packed) . companyFromTextE

instance ToHttpApiData Company where
    toUrlPiece = companyToText

-------------------------------------------------------------------------------
-- Country
-------------------------------------------------------------------------------

-- | Countries and companies are 1-to-1 at the moment.
newtype Country = Country { countryCompany :: Company }
  deriving stock (Eq, Ord, Show, Generic, Lift)
  deriving newtype (NFData, Binary, Hashable, Enum, Bounded)

instance C.PlotValue Country where
    toValue   = C.enumToValue
    fromValue = C.enumFromValue
    autoAxis  = C.enumAutoAxis $ T.takeWhile (/= '/') . countryToText

instance AnsiPretty Country

countryToText :: Country -> Text
countryToText (Country c) = (fromMaybe "No country" $ cCountry ci) <> " / " <> cName ci where
    ci = companyInfo c

countryToText' :: Country -> Text
countryToText' (Country c) = fromMaybe "No country" (cCountry ci) where
    ci = companyInfo c

countryFromText :: Text -> Maybe Country
countryFromText k = do
    m <- match re k
    case m of
        (country, Nothing) -> countryLookup ^? ix (T.toLower $ country ^. packed)
        (_, Just company)  -> Country <$> companyFromText (company ^. packed)
  where
    re = liftA2 (,) (some c) (optional $ spaces *> sym '/' *> spaces *> someFew c <* optional dot)
    c = psym (/= '/')

    someFew p = liftA2 (:) p (few p)

    spaces = many (sym ' ')
    dot = sym '.' -- For some reason we have "Futurice Ltd."

countryFromTextE :: Text -> Either String Country
countryFromTextE k =
    maybe (Left $ "Invalid country " ++ show k) Right (countryFromText k)

-------------------------------------------------------------------------------
-- Country instances
-------------------------------------------------------------------------------

instance ToHtml Country where
    toHtmlRaw = toHtml
    toHtml (Country c) = toHtml (fromMaybe "No country" $ cCountry ci) <> " / " <> toHtml c where
        ci = companyInfo c

instance Arbitrary Country where
    arbitrary = Country <$> arbitrary

instance ToParamSchema Country where
    toParamSchema _ = mempty
        & Swagger.type_ .~ Swagger.SwaggerString
        -- & Swagger.enum_ ?~ map enumToJSON_ enumUniverse_

instance ToSchema Country where
    declareNamedSchema p = pure $ Swagger.NamedSchema (Just "Country") $ mempty
        & Swagger.paramSchema .~ toParamSchema p

instance ToJSON Country where
    toJSON = Aeson.String . countryToText

instance FromJSON Country where
    parseJSON = Aeson.withTextDump "Country" $
        either (fail . view unpacked) pure . countryFromTextE

instance Csv.ToField Country where
    toField = Csv.toField . countryToText

instance Csv.FromField Country where
    parseField = Csv.parseField >=>
        either (fail . view unpacked) pure . countryFromTextE

instance FromHttpApiData Country where
    parseUrlPiece = first (view packed) . countryFromTextE

instance ToHttpApiData Country where
    toUrlPiece = countryToText
