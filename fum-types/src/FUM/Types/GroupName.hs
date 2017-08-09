{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module FUM.Types.GroupName (
    GroupName,
    groupNameToText,
    mkGroupName,
    parseGroupName,
    parseGroupName',
    groupNameRegexp,
    InvalidGroupNameFormat (..),
    ) where

import Control.Monad               ((>=>))
import Data.Aeson.Types
       (FromJSONKey (..), FromJSONKeyFunction (..), ToJSONKey (..),
       toJSONKeyText)
import Futurice.EnvConfig          (FromEnvVar (..))
import Futurice.Generics
import Futurice.Prelude
import Language.Haskell.TH         (ExpQ)
import Lucid                       (ToHtml (..))
import Prelude ()
import Text.Regex.Applicative.Text (RE', psym)

import qualified Data.Aeson.Compat                    as Aeson
import qualified Data.Csv                             as Csv
import qualified Data.Swagger                         as Swagger
import qualified Data.Text                            as T
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres
import qualified Test.QuickCheck                      as QC

-- | GroupName name. @[a-zA-Z0-9 _.-]+@.
newtype GroupName = GroupName Text
  deriving (Eq, Ord)

deriveLift ''GroupName

instance Show GroupName where
    showsPrec _ (GroupName l)
        = showString "$(mkGroupName "
        . showsPrec 11 l
        . showString ")"

-------------------------------------------------------------------------------
-- Magic
-------------------------------------------------------------------------------

mkGroupName :: Text -> ExpQ
mkGroupName t = do
    l <- parseGroupName t
    [| l |]

parseGroupName :: MonadThrow m => Text -> m GroupName
parseGroupName = either (throwM . InvalidGroupNameFormat) pure . parseGroupName'

newtype InvalidGroupNameFormat = InvalidGroupNameFormat String
  deriving (Show, Typeable)

instance Exception InvalidGroupNameFormat

-------------------------------------------------------------------------------
-- Parse
-------------------------------------------------------------------------------

groupNameToText :: GroupName -> Text
groupNameToText (GroupName l) = l

-- | Parse group name identifier
parseGroupName' :: Text -> Either String GroupName
parseGroupName' t
    | not (1 <= len)         = Left $ "group name of invalid length: " ++ show len
    | T.any isInvalidChar t  = Left $ "group name with invalid characters: " ++ show (T.take 3 (T.filter isInvalidChar t))
    | otherwise              = Right (GroupName t)
  where
    len = T.length t
    isInvalidChar = (`notElem` validChars)

validChars :: String
validChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0' .. '9'] ++ " ._-"

-- | Regexp for group name
--
-- /Note:/ use `parseGroupName` if possible, as it provides better errors.
groupNameRegexp :: RE' GroupName
groupNameRegexp = GroupName . T.pack <$> some (psym (`elem` validChars))

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Hashable GroupName where
    hashWithSalt salt (GroupName l) = hashWithSalt salt l

instance NFData GroupName where
    rnf (GroupName i) = rnf i

instance Arbitrary GroupName where
    arbitrary = QC.elements $ map GroupName [ "fooo", "booo", "hooo" ]

instance ToHtml GroupName where
    toHtmlRaw = toHtml
    toHtml = toHtml . groupNameToText

instance ToParamSchema GroupName where
    toParamSchema _ = mempty
        & Swagger.type_ .~ Swagger.SwaggerString
        -- & Swagger.enum_ ?~ map enumToJSON_ enumUniverse_

instance ToSchema GroupName where
    declareNamedSchema p = pure $ Swagger.NamedSchema (Just "GroupName") $ mempty
        & Swagger.paramSchema .~ toParamSchema p

instance ToJSON GroupName where
    toJSON = Aeson.String . groupNameToText

instance FromJSON GroupName where
    parseJSON = Aeson.withText "GroupName" $
        either fail pure . parseGroupName'

instance FromHttpApiData GroupName where
    parseUrlPiece = first (view packed) . parseGroupName'

instance ToHttpApiData GroupName where
    toUrlPiece = groupNameToText

instance ToJSONKey GroupName where
    toJSONKey = toJSONKeyText groupNameToText

instance FromJSONKey GroupName where
    fromJSONKey = FromJSONKeyTextParser $
        either fail pure . parseGroupName'

instance Csv.ToField GroupName where
    toField = Csv.toField . groupNameToText

instance Postgres.ToField GroupName where
    toField = Postgres.toField . groupNameToText

instance Postgres.FromField GroupName where
    fromField f mdata = do
        t <- Postgres.fromField f mdata
        either fail pure (parseGroupName' t)

instance FromEnvVar GroupName where
    fromEnvVar = fromEnvVar >=> parseGroupName

