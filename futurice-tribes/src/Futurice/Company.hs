{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Futurice.Company (
    Company,
    mkCompany,
    companyToText,
    companyFromText,
    _Company,
    -- * known values
    companyFuturiceOy,
    companyFuturiceLtd,
    companyFuturiceGmbH,
    companyFutuSwedenAB,
    ) where

import Control.Monad             ((>=>))
import Futurice.Company.Internal
import Futurice.Generics
import Futurice.Prelude
import Language.Haskell.TH       (ExpQ)
import Lucid                     (ToHtml (..))
import Prelude ()

import qualified Data.Csv        as Csv
import qualified Data.Map        as Map
import qualified Data.Swagger    as Swagger
import qualified Data.Text       as T
import qualified Data.Vector     as V
import qualified Futurice.Aeson  as Aeson
import qualified Test.QuickCheck as QC

-- | Company.
newtype Company = Company Int
  deriving (Eq, Ord)

deriveLift ''Company

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
