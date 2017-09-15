{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Futurice.Servant (
    -- * @main@ boilerplate
    futuriceServerMain,
    futuriceServerMain',
    futuriceNoMiddleware,
    liftFuturiceMiddleware,
    -- * HTML (lucid)
    HTML,
    -- * CSV (cassava)
    CSV,
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
    serverEnvPfx,
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
    -- * Re-export
    Job,
    ) where

import Control.Concurrent.STM
       (TVar, atomically, newTVarIO, swapTVar)
import Control.Monad.Catch                  (fromException, handleAll)
import Data.Constraint                      (Dict (..))
import Data.Swagger                         hiding (port)
import Data.TDigest.Metrics                 (registerTDigest)
import Data.Text.Encoding                   (decodeLatin1)
import Development.GitRev                   (gitCommitDate, gitHash)
import Futurice.Cache
       (CachePolicy (..), DynMapCache, cachedIO, genCachedIO)
import Futurice.Colour
       (AccentColour (..), AccentFamily (..), Colour (..), SColour)
import Futurice.EnvConfig
       (Configure, configure, envAwsCredentials, envVar, envVarWithDefault,
       getConfig', optionalAlt)
import Futurice.Lucid.Foundation            (vendorServer)
import Futurice.Metrics.RateMeter           (values)
import Futurice.Periocron
       (Job, defaultOptions, every, mkJob, spawnPeriocron)
import Futurice.Prelude
import Log.Backend.CloudWatchLogs
       (createCloudWatchLogStream, withCloudWatchLogger)
import Network.Wai
       (Middleware, requestHeaders, responseLBS)
import Network.Wai.Metrics                  (metrics, registerWaiMetrics)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Prelude ()
import Servant
import Servant.CSV.Cassava                  (CSV)
import Servant.Futurice.Favicon             (FutuFaviconAPI, serveFutuFavicon)
import Servant.Futurice.Status              hiding (info)
import Servant.HTML.Lucid                   (HTML)
import Servant.Server.Internal              (passToServer)
import Servant.Swagger
import Servant.Swagger.UI
import System.Remote.Monitoring             (forkServer, serverMetricStore)

import qualified Data.Aeson               as Aeson
import qualified FUM.Types.Login          as FUM
import qualified Futurice.DynMap          as DynMap
import qualified GHC.Stats                as Stats
import qualified Network.HTTP.Types       as H
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Metrics           as Metrics
import qualified System.Metrics.Json      as Metrics
import qualified Data.Map.Strict as Map

import qualified Network.AWS                          as AWS
import qualified Network.AWS.CloudWatch.PutMetricData as AWS
import qualified Network.AWS.CloudWatch.Types         as AWS

type FuturiceAPI api colour =
    FutuFaviconAPI colour
    :<|> api
    :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> "vendor" :> Raw
    :<|> StatusAPI

cacheStats :: DynMapCache -> StatusInfoIO
cacheStats dmap = gcStatusInfo <> dynmapStats
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
    :: forall api colour. (HasSwagger api)
    => Text  -- ^ title
    -> Text  -- ^ description
    -> DynMapCache
    -> Proxy api
    -> Server api
    -> Server (FuturiceAPI api colour)
futuriceServer t d cache papi server
    = serveFutuFavicon
    :<|> server
    :<|> swaggerSchemaUIServer (swaggerDoc t d papi)
    :<|> vendorServer
    :<|> serveStatus (cacheStats cache)

-------------------------------------------------------------------------------
-- main boilerplate
-------------------------------------------------------------------------------

-- | Data type containing the server setup
data ServerConfig f (colour :: Colour) ctx api = SC
    { _serverName        :: !Text
    , _serverDescription :: !Text
    , _serverApplication :: ctx -> Server api
    , _serverMiddleware  :: ctx -> Middleware
    , _serverEnvPfx      :: !(f Text)
    }

-- | Default server config, through the lenses the type of api will be refined
--
emptyServerConfig :: ServerConfig Proxy 'FutuGreen ctx (Get '[JSON] ())
emptyServerConfig = SC
    { _serverName         = "Futurice Service"
    , _serverDescription  = "Some futurice service"
    , _serverApplication  = \_ -> pure ()
    , _serverMiddleware   = futuriceNoMiddleware
    , _serverEnvPfx       = Proxy
    }

-- | Default middleware: i.e. nothing.
futuriceNoMiddleware :: ctx -> Middleware
futuriceNoMiddleware = liftFuturiceMiddleware id

-- | Lift config-less middleware for use with 'futuriceServerMain'.
liftFuturiceMiddleware :: Middleware -> ctx -> Middleware
liftFuturiceMiddleware mw _ = mw

serverName :: Lens' (ServerConfig f colour ctx api) Text
serverName = lens _serverName $ \sc x -> sc { _serverName = x }

serverDescription :: Lens' (ServerConfig f colour ctx api) Text
serverDescription = lens _serverDescription $ \sc x -> sc { _serverDescription = x }

serverEnvPfx :: Lens
    (ServerConfig f colour ctx api)
    (ServerConfig I colour ctx api)
    (f Text)
    Text
serverEnvPfx = lens _serverEnvPfx $ \sc x -> sc { _serverEnvPfx = I x }

serverApp
    :: Functor f
    => Proxy api'
    -> LensLike f (ServerConfig g colour ctx api) (ServerConfig g colour ctx api')
       (ctx -> Server api) (ctx -> Server api')
serverApp _ = lens _serverApplication $ \sc x -> sc { _serverApplication = x }

serverMiddleware :: Lens' (ServerConfig g colour ctx api) (ctx -> Middleware)
serverMiddleware = lens _serverMiddleware $ \sc x -> sc { _serverMiddleware = x }

serverColour
    :: Lens (ServerConfig f colour ctx api) (ServerConfig f colour' ctx api)
       (Proxy colour) (Proxy colour')
serverColour = lens (const Proxy) $ \sc _ -> coerce sc

futuriceServerMain
    :: forall cfg ctx api colour.
       (Configure cfg, HasSwagger api, HasServer api '[], SColour colour)
    => (cfg -> Logger -> DynMapCache -> IO (ctx, [Job]))
       -- ^ Initialise the context for application, add periocron jobs
    -> ServerConfig I colour ctx api
       -- ^ Server configuration
    -> IO ()
futuriceServerMain = futuriceServerMain' (\_ -> Dict)

futuriceServerMain'
    :: forall cfg ctx api colour.
       (Configure cfg, HasSwagger api, SColour colour)
    => (ctx -> Dict (HasServer api '[]))
    -> (cfg -> Logger -> DynMapCache -> IO (ctx, [Job]))
       -- ^ Initialise the context for application, add periocron jobs
    -> ServerConfig I colour ctx api
       -- ^ Server configuration
    -> IO ()
futuriceServerMain' makeDict makeCtx (SC t d server middleware (I envpfx)) =
    withStderrLogger $ \logger ->
    handleAll (handler logger) $ do
        cfg <- runLogT "futurice-servant" logger $ do
            logInfo_ $ "Hello, " <> t <> " is alive"
            getConfigWithPorts (envpfx ^. from packed)

        cache       <- newDynMapCache

        let awsGroup = fromMaybe "Haskell" (_cfgCloudWatchGroup cfg )
        let service  = t

        menv <- for (_cfgCloudWatchCreds cfg) $ \awsCreds -> do
            env' <- AWS.newEnv awsCreds
            return $ env'
                & AWS.envRegion .~ AWS.Frankfurt  -- TODO: make configurable?

        let main' = main cfg menv service cache

        case menv of
            Nothing -> main' logger
            Just env -> do
                createCloudWatchLogStream env awsGroup service
                withCloudWatchLogger env awsGroup service $ \leLogger -> main' (logger <> leLogger)

  where
    main (Cfg cfg p ekgP mgroup _) menv service cache logger = do
        (ctx, jobs)    <- makeCtx cfg logger cache
        -- brings 'HasServer' instance into a scope
        Dict           <- pure $ makeDict ctx
        let server'    =  futuriceServer t d cache proxyApi (server ctx)
                       :: Server (FuturiceAPI api colour)

        -- ekg metrics
        store      <- serverMetricStore <$> forkServer "localhost" ekgP
        waiMetrics <- registerWaiMetrics store
        -- planmill metric
        -- TODO: we register for all, make configurable
        registerTDigest "pmreq" [0.5, 0.9, 0.99] store

        statsEnabled <-
#if MIN_VERSION_base(4,10,0)
            Stats.getRTSStatsEnabled
#else
            Stats.getGCStatsEnabled
#endif
        mutgcTVar <- newTVarIO (MutGC 0 0)

        let awsGroup   = fromMaybe "Haskell" mgroup
        let mcloudwatchJob = do
                guard statsEnabled
                env <- menv
                pure (cloudwatchJob mutgcTVar logger env awsGroup service)

        let jobs' = mkJob "stats" (ekgJob logger store) (every $ 5 * 60)
                  : maybeToList (mkJob "cloudwatch" <$> mcloudwatchJob <*> pure (every 60))
                  ++ jobs
        _ <- spawnPeriocron (defaultOptions logger) store jobs'

        runLogT "futurice-servant" logger $ do
            logInfo_ $ "Starting " <> t <> " at port " <> textShow p
            logInfo_ $ "-          http://localhost:" <> textShow p <> "/"
            logInfo_ $ "- swagger: http://localhost:" <> textShow p <> "/swagger-ui/"
            logInfo_ $ "- ekg:     http://localhost:" <> textShow ekgP <> "/"

        Warp.runSettings (settings p logger)
            $ metrics waiMetrics
            $ middleware ctx
            $ serve proxyApi' server'

    cloudwatchJob :: TVar MutGC -> Logger -> AWS.Env -> Text -> Text -> IO ()
    cloudwatchJob mutgcTVar logger env awsGroup service = runLogT "cloudwatch" logger $ do
        -- averages
        meters <- liftIO values
        logInfo "Futurice.Metrics" meters
        let mkDatum (n, v) = AWS.metricDatum (n <> " (an hour window sum)")
                & AWS.mdValue      ?~ fromIntegral v
                & AWS.mdUnit       ?~ AWS.Count
                & AWS.mdDimensions .~ [AWS.dimension "Service" service]
        let meterDatums = map mkDatum (Map.toList meters)
      
        -- gcm
#if MIN_VERSION_base(4,10,0)
        stats <- liftIO Stats.getRTSStats

        let liveBytes =  Stats.gcdetails_live_bytes (Stats.gc stats)

        let currMut = Stats.mutator_cpu_ns stats
        let currTot = Stats.cpu_ns stats
#else
        stats <- liftIO Stats.getGCStats

        let liveBytes = Stats.currentBytesUsed stats

        let currMut = Stats.mutatorCpuSeconds stats
        let currTot = Stats.cpuSeconds stats
#endif

        MutGC prevMut prevTot <- liftIO $ atomically $
            swapTVar mutgcTVar (MutGC currMut currTot)

        let mutSec = currMut - prevMut
        let totSec = currTot - prevTot
        let productivity' = realToFrac mutSec / realToFrac totSec :: Double
        -- filter out invalid (e.g NaN) values
        let productivity
                | 0 <= productivity' &&  productivity' <= 100 = productivity'
                | otherwise = 0

        -- TODO: create outside the job?

        rs <- liftIO $ AWS.runResourceT $ AWS.runAWS env $ do

            -- Residency
            let datum = AWS.metricDatum "Live bytes"
                    & AWS.mdValue      ?~ fromIntegral liveBytes
                    & AWS.mdUnit       ?~ AWS.Bytes
                    & AWS.mdDimensions .~ [AWS.dimension "Service" service]
            -- Productivity
            let datum2 = AWS.metricDatum "Productivity"
                    & AWS.mdValue      ?~ productivity
                    & AWS.mdUnit       ?~ AWS.Percent
                    & AWS.mdDimensions .~ [AWS.dimension "Service" service]

            -- Put.
            let pmd = AWS.putMetricData (awsGroup <> "/RTS")
                    & AWS.pmdMetricData .~ datum : datum2 : meterDatums

            AWS.send pmd

        logInfo_ $ "cloudwatch response " <> textShow rs

    ekgJob :: Logger -> Metrics.Store -> IO ()
    ekgJob logger store = runLogT "ekg" logger $ do
        sample <- liftIO $ Metrics.sampleAll store
        logInfo "ekg sample" (Metrics.sampleToJson sample)

    handler logger e = do
        runLogT "futurice-servant" logger $ logAttention_ $ textShow e
        throwM e

    settings p logger = Warp.defaultSettings
        & Warp.setPort p
        & Warp.setOnException (onException logger)
        & Warp.setOnExceptionResponse onExceptionResponse
        & Warp.setServerName (encodeUtf8 t)

    onException logger mreq e = do
        runLogT "warp" logger $ do
            logAttention (textShow e) mreq

    -- On exception return JSON
    -- TODO: we could return some UUID and log exception with it.
    -- but maybe it's worth doing only when errors are rare.
    onExceptionResponse e = responseLBS
        s
        [(H.hContentType, "application/json; charset=utf-8")]
        (Aeson.encode ("Something went wrong" :: Text))
      where
        s = case fromException e :: Maybe Warp.InvalidRequest of
            Just _  -> H.badRequest400
            Nothing -> H.internalServerError500

    proxyApi :: Proxy api
    proxyApi = Proxy

    proxyApi' :: Proxy (FuturiceAPI api colour)
    proxyApi' = Proxy

-------------------------------------------------------------------------------
-- MutGC
-------------------------------------------------------------------------------

#if MIN_VERSION_base(4,10,0)
data MutGC = MutGC !Stats.RtsTime !Stats.RtsTime
#else
data MutGC = MutGC !Double !Double
#endif

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
    type ServerT (SSOUser :> api) m = Maybe FUM.Login -> ServerT api m

    route Proxy context subserver =
        route (Proxy :: Proxy api) context (passToServer subserver ssoUser)
      where
        ssoUser req = do
            l <- lookup "REMOTE-USER" (requestHeaders req)
            FUM.parseLogin . decodeLatin1 $ l

instance HasLink api => HasLink (SSOUser :> api) where
    type MkLink (SSOUser :> api) = MkLink api
    toLink _ = toLink (Proxy :: Proxy api)

instance HasSwagger api => HasSwagger (SSOUser :> api) where
    toSwagger _ = toSwagger (Proxy :: Proxy api)

-------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------

data Cfg cfg = Cfg
    { _cfgInner           :: !cfg
    , _cfgPort            :: !Int
    , _cfgEkgPort         :: !Int
    , _cfgCloudWatchGroup :: !(Maybe Text)
    , _cfgCloudWatchCreds :: !(Maybe AWS.Credentials)
    }
  deriving Show

getConfigWithPorts
    :: (MonadLog m, MonadIO m, Configure cfg)
    => String
    -> m (Cfg cfg)
getConfigWithPorts n = getConfig' n $ Cfg
    <$> configure
    <*> envVarWithDefault "PORT" defaultPort
    <*> envVarWithDefault "EKGPORT" defaultEkgPort
    <*> optionalAlt (envVar "CLOUDWATCH_GROUP")
    <*> optionalAlt (envAwsCredentials "CLOUDWATCH_")

-------------------------------------------------------------------------------
-- Defaults
-------------------------------------------------------------------------------

defaultPort :: Int
defaultPort = 8000

defaultEkgPort :: Int
defaultEkgPort = 9000
