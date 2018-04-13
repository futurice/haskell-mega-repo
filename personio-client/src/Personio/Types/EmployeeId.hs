{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Personio.Types.EmployeeId where

import Data.Aeson
import Futurice.Constants (personioPublicUrl)
import Futurice.Generics
import Futurice.Prelude
import Lucid              (ToHtml (..), a_, class_, href_)
import Prelude ()

import qualified Data.Csv as Csv

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

instance ToHtml EmployeeId where
    toHtmlRaw = toHtml
    toHtml (EmployeeId i) = do
        let t = textShow i
        a_ [ class_ "personio", href_ $ personioPublicUrl <> "/staff/details/" <> t ] $
            toHtml t

_EmployeeId :: Prism' Text EmployeeId
_EmployeeId = prism' toUrlPiece (either (const Nothing) Just . parseUrlPiece)
