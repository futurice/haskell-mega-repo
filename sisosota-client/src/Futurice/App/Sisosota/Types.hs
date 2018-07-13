{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Sisosota.Types (
    sisosotaMaxUploadSize,
    ContentHash,
    contentHashToText,
    contentHashFromText,
    contentHashData,
    contentHashLBS,
    ContentData (..),
    mkContentData,
  ) where

import Data.Aeson                           (FromJSON (..), ToJSON (..))
import Data.Swagger
       (NamedSchema (..), ToParamSchema (..), ToSchema (..))
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Futurice.Prelude
import Lucid                                (ToHtml (..))
import Prelude ()
import Servant.API
       (FromHttpApiData (..), MimeRender (..), MimeUnrender (..), OctetStream,
       ToHttpApiData (..))

import qualified Crypto.Hash.SHA512                   as SHA512
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Base64.URL           as Base64
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.Swagger                         as Swagger
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

sisosotaMaxUploadSize :: Int
sisosotaMaxUploadSize = 4 * 1024 * 1024 -- ~ 4MiB

-------------------------------------------------------------------------------
-- ContentHash
-------------------------------------------------------------------------------

-- | SHA512 hash of the data
--
-- /Invariant:/ 'ByteString' is 64 bytes long
newtype ContentHash = ContentHash ByteString
  deriving (Show)

contentHashToText :: ContentHash -> Text
contentHashToText (ContentHash bs) = decodeUtf8Lenient $ Base64.encode bs

contentHashFromText :: Text -> Either String ContentHash
contentHashFromText = fmap ContentHash . Base64.decode . encodeUtf8

instance Textual ContentHash where
    textualToText   = contentHashToText
    textualFromText = contentHashFromText

deriveVia [t| ToJSON ContentHash             `Via` Textica ContentHash |]
deriveVia [t| FromJSON ContentHash           `Via` Textica ContentHash |]
deriveVia [t| ToHttpApiData ContentHash      `Via` Textica ContentHash |]
deriveVia [t| FromHttpApiData ContentHash    `Via` Textica ContentHash |]
deriveVia [t| Postgres.ToField ContentHash   `Via` Textica ContentHash |]
deriveVia [t| Postgres.FromField ContentHash `Via` Textica ContentHash |]
deriveVia [t| ToHtml ContentHash             `Via` Textica ContentHash |]

instance ToParamSchema ContentHash where toParamSchema = textualToParamSchema
instance ToSchema ContentHash where declareNamedSchema = textualDeclareNamedSchema

contentHashData :: ContentData -> ContentHash
contentHashData (ContentData lbs) = contentHashLBS lbs

contentHashLBS :: LazyByteString -> ContentHash
contentHashLBS = ContentHash . SHA512.hashlazy

instance ToHtml ContentHash where
    toHtmlRaw = toHtml
    toHtml = toHtml . contentHashToText

instance ToHttpApiData ContentHash where
    toUrlPiece = contentHashToText

instance FromHttpApiData ContentHash where
    parseUrlPiece t = do
        t' <- parseUrlPiece t
        either (fail . view packed) pure $ contentHashFromText t'

instance ToParamSchema ContentHash where
    toParamSchema _ = mempty
        & Swagger.type_  .~ Swagger.SwaggerString
        & Swagger.format ?~ "64 bytes encoded in URL base64"

instance ToSchema ContentHash where
    declareNamedSchema p =
        pure $ Swagger.NamedSchema (Just "ContentHash") $ Swagger.paramSchemaToSchema p
            & Swagger.example ?~ toJSON (contentHashLBS "foobar")

instance FromJSON ContentHash where
    parseJSON v = do
        t <- parseJSON v
        either fail pure $ contentHashFromText t

instance ToJSON ContentHash where
    toJSON     = toJSON . contentHashToText
    toEncoding = toEncoding . contentHashToText

instance ToField ContentHash where
    toField = toField . contentHashToText

instance FromField ContentHash where
    fromField a b = do
        fieldText <- fromField a b
        case contentHashFromText fieldText of
          Right contentHash -> pure contentHash
          Left _error -> empty

-------------------------------------------------------------------------------
-- ContentData
-------------------------------------------------------------------------------

newtype ContentData = ContentData LazyByteString

-- | Reads lazy 'LBS.ByteString' until 'sisosotaMaxUploadSize'.
mkContentData :: LazyByteString -> Either String ContentData
mkContentData lbs
    = runEitherK
    $ fmap (ContentData . LBS.fromChunks)
    $ go 0
    $ LBS.toChunks lbs
  where
    go !_   [] = EK ($ []) -- pure []
    go !acc (bs : bss) | acc' < sisosotaMaxUploadSize =
        fmap (bs :) (go acc' bss)
      where
        acc' = acc + BS.length bs
    go !acc _ = throwEitherK $ "Big upload, read " ++ show acc ++ " bytes"

instance MimeRender OctetStream ContentData where
    mimeRender p (ContentData bs) = mimeRender p bs

instance MimeUnrender OctetStream ContentData where
    mimeUnrender _ = mkContentData

instance ToSchema ContentData where
    declareNamedSchema _ = pure $ NamedSchema (Just "Byte data") $ mempty
        & Swagger.description ?~ "Raw data"

-------------------------------------------------------------------------------
-- Helper
-------------------------------------------------------------------------------

-- from unification-fd
newtype EitherK e a = EK (forall r. (a -> Either e r) -> Either e r)

instance Functor (EitherK e) where
    fmap f (EK k) = EK $ \r -> k (r . f)

runEitherK :: EitherK e a -> Either e a
runEitherK (EK k) = k Right

throwEitherK :: e -> EitherK e a
throwEitherK e = EK (\_ -> Left e)
