{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.App.Preferences.API where

import FUM.Types.Login                (Login)
import Futurice.App.Preferences.Types
import Futurice.Prelude
import Prelude ()
import Servant

type PreferencesAPI =
    Summary "Get preferences"
        :> Description "Get preferences"
        :> "api" :> "preferences" :> ReqBody '[JSON] [Login] :> Post '[JSON] (Map Login Preferences)

preferencesApi :: Proxy PreferencesAPI
preferencesApi = Proxy
