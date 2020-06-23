{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Personio.Types.EmployeeId where

import Data.Aeson
import Futurice.Constants (personioPublicUrl)
import Futurice.EnvConfig (FromEnvVar (..), FromEnvVarList (..))
import Futurice.Generics
import Futurice.Prelude
import Lucid              (ToHtml (..), a_, class_, href_)
import Prelude ()

import qualified Data.Csv                             as Csv
import qualified Data.Text                            as T
import qualified Database.PostgreSQL.Simple.FromField as Postgres
import qualified Database.PostgreSQL.Simple.ToField   as Postgres

-- | Personio employee id.
newtype EmployeeId = EmployeeId Word
  deriving stock (Eq, Ord, Show)
  deriving newtype
    ( Arbitrary, Hashable, NFData
    , FromJSON, ToJSON, FromJSONKey, ToJSONKey
    , FromHttpApiData, ToHttpApiData, Csv.ToField, Csv.FromField
    )

deriveGeneric ''EmployeeId

-- deriving via
instance ToParamSchema EmployeeId where toParamSchema = newtypeToParamSchema
instance ToSchema EmployeeId where declareNamedSchema = newtypeDeclareNamedSchema

-- | There are now FromField Word instance
--
-- We use 'Integer' underlying instance, which may over and underflow.
instance Postgres.FromField EmployeeId where
    fromField f mbs = EmployeeId . fromInteger <$> Postgres.fromField f mbs

instance Postgres.ToField EmployeeId where
    toField = Postgres.toField . toInteger . (coerce :: EmployeeId -> Word)

instance ToHtml EmployeeId where
    toHtmlRaw = toHtml
    toHtml (EmployeeId i) = do
        let t = textShow i
        a_ [ class_ "personio", href_ $ personioPublicUrl <> "/staff/details/" <> t ] $
            toHtml t

instance FromEnvVar EmployeeId where
    fromEnvVar eid = EmployeeId <$> readMaybe eid

instance FromEnvVarList EmployeeId where
    fromEnvVarList eids = (fmap . fmap) EmployeeId $ traverse (readMaybe . T.unpack) (T.splitOn "," $ T.pack eids)


_EmployeeId :: Prism' Text EmployeeId
_EmployeeId = prism' toUrlPiece (either (const Nothing) Just . parseUrlPiece)
