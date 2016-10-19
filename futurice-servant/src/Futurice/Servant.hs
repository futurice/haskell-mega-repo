{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.Servant (
    -- * @main@ boilerplate
    futuriceServerMain,
    futuriceNoMiddleware,
    liftFuturiceMiddleware,
    -- * HTML (lucid)
    HTML,
    -- * Swagger
    -- | These are useful for defining empty schemas
    --
    -- @
    -- instance 'ToSchema' IndexPage where
    --     declareNamedSchema _ = pure $ 'NamedSchema' (Just "Indexpage") mempty
    -- @
    --
    NamedSchema (..), ToSchema (..), ToParamSchema (..),
    -- * Favicon
    Colour (..),
    AccentColour (..),
    AccentFamily (..),
    -- * SSO user
    SSOUser,
    -- * Lower-level
    -- ** Server API
    FuturiceAPI,
    futuriceServer,
    ServerConfig,
    emptyServerConfig,
    serverName,
    serverDescription,
    serverApp,
    serverMiddleware,
    serverColour,
    -- ** WAI
    Application,
    Middleware,
    -- ** Cache
    DynMapCache,
    newDynMapCache,
    cachedIO,
    genCachedIO,
    CachePolicy(..),
    -- * Middlewares
    logStdoutDev,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Concurrent.STM               (atomically)
import Control.Lens                         (Lens, LensLike)
import Data.Char                            (isAlpha)
import Data.Swagger                         hiding (HasPort (..))
import Development.GitRev                   (gitCommitDate, gitHash)
import Futurice.Colour
       (AccentColour (..), AccentFamily (..), Colour (..), SColour)
import Futurice.EnvConfig                   (GetConfig (..), HasPort (..))
import GHC.Prim                             (coerce)
import Network.Wai                          (Middleware, requestHeaders)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant
import Servant.Cache.Class
       (CachePolicy (..), DynMapCache, cachedIO, genCachedIO)
import Servant.Futurice.Favicon             (FutuFaviconAPI, serveFutuFavicon)
import Servant.Futurice.Status              hiding (info)
import Servant.HTML.Lucid                   (HTML)
import Servant.Server.Internal              (passToServer)
import Servant.Swagger
import Servant.Swagger.UI
import System.IO                            (stderr)

import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE
import qualified Data.Text.IO                  as T
import qualified FUM
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Servant.Cache.Internal.DynMap as DynMap

type FuturiceAPI api colour =
    FutuFaviconAPI colour
    :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> StatusAPI
    :<|> api

stats :: DynMapCache -> StatusInfoIO
stats dmap = gcStatusInfo <> dynmapStats
  where
    dynmapStats :: StatusInfoIO
    dynmapStats = SIIO $ group "cache" . metric "size" <$> dynmapSize

    dynmapSize :: IO Int
    dynmapSize = atomically $ DynMap.size dmap

swaggerDoc
    :: HasSwagger api
    => Text  -- ^ title
    -> Text  -- ^ description
    -> Proxy api
    -> Swagger
swaggerDoc t d proxy = toSwagger proxy
    & info.title       .~ t
    & info.version     .~ fromString v
    & info.version     .~ fromString v
    & info.description ?~ d
  where
    v = $(gitCommitDate) ++ " " ++ $(gitHash)

-- | Create futurice server
futuriceServer
    :: forall api colour. (HasSwagger api, SColour colour)
    => Text  -- ^ title
    -> Text  -- ^ description
    -> DynMapCache
    -> Proxy api
    -> Server api
    -> Server (FuturiceAPI api colour)
futuriceServer t d cache papi server
    = serveFutuFavicon
    :<|> swaggerSchemaUIServer (swaggerDoc t d papi)
    :<|> serveStatus (stats cache)
    :<|> server

-------------------------------------------------------------------------------
-- main boilerplate
-------------------------------------------------------------------------------

-- | Data type containing the server setup
data ServerConfig (colour :: Colour) ctx api = SC
    { _serverName        :: !Text
    , _serverDescription :: !Text
    , _serverApplication :: ctx -> Server api
    , _serverMiddleware  :: ctx -> Middleware
    }

-- | Default server config, through the lenses the type of api will be refined
--
emptyServerConfig :: ServerConfig 'FutuGreen ctx (Get '[JSON] ())
emptyServerConfig = SC
    { _serverName         = "Futurice Service"
    , _serverDescription  = "Some futurice service"
    , _serverApplication  = \_ -> pure ()
    , _serverMiddleware   = futuriceNoMiddleware
    }

-- | Default middleware: i.e. nothing.
futuriceNoMiddleware :: ctx -> Middleware
futuriceNoMiddleware = liftFuturiceMiddleware id

-- | Lift config-less middleware for use with 'futuriceServerMain'.
liftFuturiceMiddleware :: Middleware -> ctx -> Middleware
liftFuturiceMiddleware mw _ = mw

serverName :: Lens' (ServerConfig colour ctx api) Text
serverName = lens _serverName $ \sc x -> sc { _serverName = x }

serverDescription :: Lens' (ServerConfig colour ctx api) Text
serverDescription = lens _serverDescription $ \sc x -> sc { _serverDescription = x }

serverApp
    :: Functor f
    => Proxy api'
    -> LensLike f (ServerConfig colour ctx api) (ServerConfig colour ctx api')
       (ctx -> Server api) (ctx -> Server api')
serverApp _ = lens _serverApplication $ \sc x -> sc { _serverApplication = x }

serverMiddleware :: Lens' (ServerConfig colour ctx api) (ctx -> Middleware)
serverMiddleware = lens _serverMiddleware $ \sc x -> sc { _serverMiddleware = x }

serverColour
    :: Lens (ServerConfig colour ctx api) (ServerConfig colour' ctx api)
       (Proxy colour) (Proxy colour')
serverColour = lens (const Proxy) $ \sc _ -> coerce sc

-- TODO: make class for config, to get ekg port later
futuriceServerMain
    :: forall cfg ctx api colour.
       (GetConfig cfg, HasPort cfg, HasSwagger api, HasServer api '[], SColour colour)
    => (cfg -> DynMapCache -> IO ctx)
       -- ^ Initialise the context for application
    -> ServerConfig colour ctx api
       -- ^ Server configuration
    -> IO ()
futuriceServerMain makeCtx (SC t d server middleware)  = do
    let cfgPort = view port
    T.hPutStrLn stderr $ "Hello, " <> t <> " is alive"
    cfg         <- getConfig
    let p       = cfgPort cfg
    cache       <- newDynMapCache
    ctx         <- makeCtx cfg cache
    let server' = futuriceServer t d cache proxyApi (server ctx)
                :: Server (FuturiceAPI api colour)
    T.hPutStrLn stderr $ "Starting " <> t <> " at port " <> show p ^. packed
    T.hPutStrLn stderr $ "- http://localhost:" <> show p ^. packed <> "/"
    T.hPutStrLn stderr $ "- http://localhost:" <> show p ^. packed <> "/swagger-ui/"
    Warp.run p $ middleware ctx $ serve proxyApi' server'
  where
    proxyApi :: Proxy api
    proxyApi = Proxy

    proxyApi' :: Proxy (FuturiceAPI api colour)
    proxyApi' = Proxy

-------------------------------------------------------------------------------
-- Other stuff
-------------------------------------------------------------------------------

newDynMapCache :: IO DynMapCache
newDynMapCache = DynMap.newIO

-------------------------------------------------------------------------------
-- SSO User
-------------------------------------------------------------------------------

data SSOUser

instance HasServer api context => HasServer (SSOUser :> api) context where
    type ServerT (SSOUser :> api) m = Maybe FUM.UserName -> ServerT api m

    route Proxy context subserver =
        route (Proxy :: Proxy api) context (passToServer subserver ssoUser)
      where
        ssoUser req = FUM.UserName . T.filter isAlpha . TE.decodeLatin1 <$>
            lookup "HTTP_REMOTE_USER" (requestHeaders req)

instance HasLink api => HasLink (SSOUser :> api) where
    type MkLink (SSOUser :> api) = MkLink api
    toLink _ = toLink (Proxy :: Proxy api)

instance HasSwagger api => HasSwagger (SSOUser :> api) where
    toSwagger _ = toSwagger (Proxy :: Proxy api)
