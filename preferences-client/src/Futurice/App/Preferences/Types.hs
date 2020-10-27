{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE InstanceSigs    #-}
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
  deriving stock (Eq, Show, GhcGeneric)
  deriving anyclass (SopGeneric, HasDatatypeInfo)
  deriving (ToJSON, FromJSON) via (Sopica Preferences)

defaultPreferences :: Preferences
defaultPreferences = Preferences
    { _prefHoursPingSMS   = True
    , _prefHoursPingEmail = True
    }

makeLenses ''Preferences

instance ToSchema Preferences where declareNamedSchema = sopDeclareNamedSchema
