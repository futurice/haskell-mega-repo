module Futurice.CommandResponse where

import Data.Aeson       (FromJSON, ToJSON)
import Futurice.Prelude
import Prelude ()

import qualified Data.Swagger

data CommandResponse a
    = CommandResponseOk a            -- ^ Do nothing
    | CommandResponseError String    -- ^ an error
    | CommandResponseReload          -- ^ reload current page
    | CommandResponseRedirect !Text  -- ^ redirect to the url
  deriving (Eq, Ord, Show, Typeable, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (CommandResponse a)
instance FromJSON a => FromJSON (CommandResponse a)
instance Data.Swagger.ToSchema a => Data.Swagger.ToSchema (CommandResponse a) where
    declareNamedSchema = Data.Swagger.genericDeclareNamedSchemaUnrestricted Data.Swagger.defaultSchemaOptions
