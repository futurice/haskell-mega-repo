{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
module Futurice.App.Preferences.Types where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

data Preferences = Preferences
    { _prefHoursPingSMS   :: !Bool
    , _prefHoursPingEmail :: !Bool
    }
  deriving stock (Eq, Show, Generic)

defaultPreferences :: Preferences
defaultPreferences = Preferences
    { _prefHoursPingSMS   = True
    , _prefHoursPingEmail = True
    }

makeLenses ''Preferences
deriveGeneric ''Preferences

deriveVia [t| ToJSON Preferences   `Via` Sopica Preferences |]
deriveVia [t| FromJSON Preferences `Via` Sopica Preferences |]

instance ToSchema Preferences where declareNamedSchema = sopDeclareNamedSchema
