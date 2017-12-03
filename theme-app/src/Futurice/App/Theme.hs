{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Theme (defaultMain) where

import Futurice.Prelude
import Prelude ()

import Futurice.Servant
import Network.Wai.Application.Static (embeddedSettings, staticApp)
import Servant
import Servant.Swagger.UI.Internal    (mkRecursiveEmbedded)

import Futurice.App.Theme.API
import Futurice.App.Theme.Config
import Futurice.App.Theme.Markup

-- | API server
server :: () -> Server ThemeAPI
server _ = pure indexPage :<|> static

static :: Server Raw
static = Tagged $ staticApp $ embeddedSettings $(mkRecursiveEmbedded "images")

defaultMain :: IO ()
defaultMain = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverName         .~ "Theme app"
    & serverDescription  .~ "Futurice theme guidelines"
    & serverColour       .~ (Proxy :: Proxy 'FutuGreen)
    & serverApp themeApi .~ server
    & serverEnvPfx       .~ "THEMEAPP"
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> IO ((), [Job])
    makeCtx _ _ _ _ = pure ((), [])
