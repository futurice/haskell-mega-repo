{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Theme.Main (defaultMain) where

import FileEmbedLzma                  (embedRecursiveDir)
import Futurice.Prelude
import Futurice.Servant
import Network.Wai.Application.Static (embeddedSettings, staticApp)
import Prelude ()
import Servant

import Futurice.App.Theme.API
import Futurice.App.Theme.Config
import Futurice.App.Theme.Markup

-- | API server
server :: () -> Server ThemeAPI
server _ = pure . indexPage
    :<|> public

public :: Server Raw
public = Tagged $ staticApp $ embeddedSettings $(embedRecursiveDir "public")

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ (emptyServerConfig
    & serverService      .~ ThemeService
    & serverDescription  .~ "Futurice theme guidelines"
    & serverColour       .~ (Proxy :: Proxy 'FutuGreen)
    & serverApp themeApi .~ server
    & serverEnvPfx       .~ "THEMEAPP" :: ServerConfig I I () 'FutuGreen () EmptyAPI ThemeAPI)
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> MessageQueue -> IO ((), [Job])
    makeCtx _ _ _ _ _ = pure ((), [])
