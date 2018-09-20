{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module FUM.Types.Login (
    Login,
    loginToText,
    mkLogin,
    parseLogin,
    parseLogin',
    loginKleene,
    loginRegexp,
    InvalidLoginFormat (..),
    ) where

import Control.Monad               ((>=>))
import Data.Aeson.Types
       (FromJSONKey (..), FromJSONKeyFunction (..), ToJSONKey (..),
       toJSONKeyText)
import Futurice.Constants          (fumPublicUrl)
import Futurice.EnvConfig          (FromEnvVar (..))
import Futurice.Generics
import Futurice.Prelude
import Language.Haskell.TH         (ExpQ)
import Lucid                       (ToHtml (..), a_, class_, href_)
import Prelude ()
import Text.Regex.Applicative.Text (RE')

import qualified Codec.Serialise                      as S
import qualified Data.Aeson.Compat                    as Aeson
import qualified Data.Csv                             as Csv
import qualified Data.Swagger                         as Swagger
import qualified Data.Text                            as T
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres
import qualified Kleene.Functor                       as K
import qualified Test.QuickCheck                      as QC

-- | Login name. @[a-z]{4,5}|itteam@.
newtype Login = Login Text
  deriving (Eq, Ord)

deriveLift ''Login

instance Show Login where
    showsPrec _ (Login l)
        = showString "$(mkLogin "
        . showsPrec 11 l
        . showString ")"

-------------------------------------------------------------------------------
-- Magic
-------------------------------------------------------------------------------

mkLogin :: Text -> ExpQ
mkLogin t = do
    l <- parseLogin t
    [| l |]

parseLogin :: MonadThrow m => Text -> m Login
parseLogin = either (throwM . InvalidLoginFormat) pure . parseLogin'

newtype InvalidLoginFormat = InvalidLoginFormat String
  deriving (Show, Typeable)

instance Exception InvalidLoginFormat

-------------------------------------------------------------------------------
-- Parse
-------------------------------------------------------------------------------

loginToText :: Login -> Text
loginToText (Login l) = l

-- | Parse login identifier
parseLogin' :: Text -> Either String Login
parseLogin' t
    | t == "itteam"              = Right (Login t)
    | not (4 <= len && len <= 5) = Left $ "login of invalid length: " ++ show len ++ " " ++ T.take 10 t ^. unpacked
    | T.any isInvalidChar t      = Left $ "login with invalid characters: " ++ show (T.take 3 (T.filter isInvalidChar t))
    | otherwise                  = Right (Login t)
  where
    len = T.length t
    isInvalidChar = (`notElem` ['a'..'z'])

-- | Login's regular expression.
--
-- >>> K.putPretty loginKleene
-- ^[a-z][a-z][a-z][a-z][a-z]?$
--
loginKleene :: K.K Char Login
loginKleene = Login . T.pack <$> range 4 5 (K.charRange 'a' 'z')
  where
    range
        :: Alternative f
        => Int  -- ^ min
        -> Int  -- ^ max
        -> f a
        -> f [a]
    range mi ma f = go mi ma
      where
        go start end
            | start > end || end <= 0 = pure []
            | start > 0 = (:) <$> f <*> go (start - 1) (end - 1)
            | otherwise = inRange <$> optional f <*> go 0 (end - 1)

        inRange current next = maybe [] (:next) current

-- | Regexp for login identifier
--
-- /Notes:/
--
-- * use `parseLogin` if possible, as it provides better errors.
--
-- * this is strict @[a-z]{4,5}@ regexp.
--
loginRegexp :: RE' Login
loginRegexp = K.toRA loginKleene

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance Hashable Login where
    hashWithSalt salt (Login l) = hashWithSalt salt l

instance NFData Login where
    rnf (Login i) = rnf i

instance Arbitrary Login where
    arbitrary = QC.elements $ map Login [ "fooo", "booo", "hooo" ]

-- | Prints link to FUM.
-- Use 'loginToText' if you need to markup 'Login' differently.
instance ToHtml Login where
    toHtmlRaw = toHtml
    toHtml (Login l) =
        a_ [ class_ "login" , href_ $ fumPublicUrl <> "/fum/users/" <> l ] $
          toHtml l

instance ToParamSchema Login where
    toParamSchema _ = mempty
        & Swagger.type_ .~ Swagger.SwaggerString
        -- & Swagger.enum_ ?~ map enumToJSON_ enumUniverse_

instance ToSchema Login where
    declareNamedSchema p = pure $ Swagger.NamedSchema (Just "Login") $ mempty
        & Swagger.paramSchema .~ toParamSchema p

instance ToJSON Login where
    toJSON = Aeson.String . loginToText

instance FromJSON Login where
    parseJSON = Aeson.withText "Login" $
        either fail pure . parseLogin'

instance FromHttpApiData Login where
    parseUrlPiece = first (view packed) . parseLogin'

instance ToHttpApiData Login where
    toUrlPiece = loginToText

instance ToJSONKey Login where
    toJSONKey = toJSONKeyText loginToText

instance FromJSONKey Login where
    fromJSONKey = FromJSONKeyTextParser $
        either fail pure . parseLogin'

instance Csv.ToField Login where
    toField = Csv.toField . loginToText

instance Postgres.ToField Login where
    toField = Postgres.toField . loginToText

instance Postgres.FromField Login where
    fromField f mdata = do
        t <- Postgres.fromField f mdata
        either fail pure (parseLogin' t)

instance FromEnvVar Login where
    fromEnvVar = fromEnvVar >=> parseLogin

instance S.Serialise Login where
    encode (Login l) = S.encode l
    decode           = do
        l <- S.decode
        either fail return $ parseLogin' l

-- $setup
-- >>> import qualified Kleene.Internal.Pretty as K
