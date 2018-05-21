{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.Email (
    Email,
    mkEmail,
    emailToText,
    emailFromText,
    emailKleene,
    emailRegexp,
    ) where

import Data.Aeson
       (FromJSON (..), FromJSONKey (..), FromJSONKeyFunction (..), ToJSON (..),
       ToJSONKey (..), withText)
import Futurice.EnvConfig          (FromEnvVar (..))
import Futurice.Prelude
import Language.Haskell.TH         (ExpQ)
import Lucid                       (ToHtml (..), a_, href_)
import Prelude ()
import Test.QuickCheck             (Arbitrary (..))
import Text.Regex.Applicative.Text (RE', match)
import Web.HttpApiData             (FromHttpApiData (..), ToHttpApiData (..))

import qualified Kleene.Functor as K
import qualified Data.Csv     as Csv
import qualified Data.Swagger as S
import qualified Data.Text    as T

-- | Futurice email. i.e. @someone@@futurice.com@.
newtype Email = Email Text
  deriving (Eq, Ord, Show)

deriveLift ''Email

emailToText :: Email -> Text
emailToText (Email x) = x <> suffix

emailFromText :: Text -> Maybe Email
emailFromText = match emailRegexp

parseEmail :: Monad m => Text -> m Email
parseEmail t = maybe
    (fail $ "Invalid Futurice email: " <> show t)
    pure
    (emailFromText t)

-------------------------------------------------------------------------------
-- Template Haskell
-------------------------------------------------------------------------------

-- | Create email at compile time.
mkEmail :: String -> ExpQ
mkEmail n
    | Just t <- emailFromText (n ^. packed) = [| t |]
    | otherwise = fail $ "Invalid futurice email name: " ++ n

-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------

instance Arbitrary Email where
    arbitrary = pure (Email "arbitrary")

instance NFData Email where
    rnf (Email x) = rnf x

instance Binary Email where
    put (Email x) = put x
    get = Email <$> get

instance Hashable Email where
    hashWithSalt salt (Email x) = hashWithSalt salt x

instance ToHtml Email where
    toHtmlRaw = toHtml
    toHtml (Email x) = a_ [ href_ $ "mailto:" <> x' ] $ toHtml x'
      where
        x' = x <> suffix

instance S.ToParamSchema Email where
    toParamSchema _ = mempty
        & S.type_  .~ S.SwaggerString
        & S.format ?~ "futurice-email"

instance S.ToSchema Email where
    declareNamedSchema p = pure $ S.NamedSchema (Just "Futurice email") $
        S.paramSchemaToSchema p

instance ToJSON Email where
    toJSON = toJSON . emailToText

instance FromJSON Email where
    parseJSON = withText "Email" parseEmail

instance ToJSONKey Email where
    toJSONKey = emailToText >$< toJSONKey

instance FromJSONKey Email where
    fromJSONKey = FromJSONKeyTextParser parseEmail

instance ToHttpApiData Email where
    toUrlPiece = emailToText

instance FromHttpApiData Email where
    parseUrlPiece = parseEmail

instance Csv.ToField Email where
    toField = Csv.toField . emailToText

instance Csv.FromField Email where
    parseField x = Csv.parseField x >>= parseEmail

instance FromEnvVar Email where
    fromEnvVar s = emailFromText (s ^. packed)

-------------------------------------------------------------------------------
-- helpers
-------------------------------------------------------------------------------

emailKleene :: K.K Char Email
emailKleene = Email . T.toLower . view packed
    <$> K.everything1
    <* (suffix :: K.K Char String)

emailRegexp :: RE' Email
emailRegexp = K.toRA emailKleene

suffix :: IsString a => a
suffix = "@futurice.com"
