{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeOperators     #-}
module Main (main) where

import Prelude ()
import Futurice.Prelude
import Codec.Picture       (Image, PixelRGBA8)
import Futurice.Colour
import Futurice.EnvConfig  (Configure (..))
import Futurice.Logo
import Futurice.Servant
import Lucid               hiding (for_)
import Servant
import Servant.JuicyPixels (PNG)

type IconAPI = "icon" :> Capture "colour" Colour :> Get '[PNG] (Image PixelRGBA8)

type API =
    Get '[HTML] IndexPage
    :<|> IconAPI

api :: Proxy API
api = Proxy

iconEndpoint :: Proxy IconAPI
iconEndpoint = Proxy

data Config = Config
type Ctx = (Logger, Cache)

instance Configure Config where
    configure = pure Config

server :: Ctx -> Server API
server (logger, cache) = pure IndexPage :<|> liftIO . makeLogo'
 where
   makeLogo' c = cachedIO logger cache 3600 c (evaluate $!! makeLogo c)

main :: IO ()
main = futuriceServerMain (const makeCtx) $ emptyServerConfig
    & serverName        .~ "Favicon API"
    & serverDescription .~ "Futurice favicons"
    & serverColour      .~ (Proxy :: Proxy 'FutuBlack)
    & serverApp api     .~ server
    & serverEnvPfx      .~ "FAVICON"
  where
    makeCtx :: Config -> Logger -> Manager -> Cache -> IO (Ctx, [Job])
    makeCtx _cfg logger _mgr cache = return ((logger, cache), [])

-------------------------------------------------------------------------------
-- IndexPage
-------------------------------------------------------------------------------

data IndexPage = IndexPage

instance ToHtml IndexPage where
    toHtmlRaw = toHtml
    toHtml _ = doctypehtml_ $ for_ [minBound..maxBound] $ \colour ->
        let link = textShow $ safeLink api iconEndpoint colour
        in a_ [href_ link] $ img_ [src_ link]

instance ToSchema IndexPage where
    declareNamedSchema _ = pure $ NamedSchema (Just "Indexpage") mempty
