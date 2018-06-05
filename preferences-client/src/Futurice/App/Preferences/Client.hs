module Futurice.App.Preferences.Client (getPreferences) where

import Futurice.Prelude
import Prelude ()
import Servant.Client
import FUM.Types.Login (Login)

import Futurice.App.Preferences.API
import Futurice.App.Preferences.Types

getPreferences :: Manager -> BaseUrl -> [Login] -> IO (Map Login Preferences)
getPreferences mgr burl req = do
    res <- runClientM (impl req) (mkClientEnv mgr burl)
    either throwM return res
  where
    impl = client preferencesApi
