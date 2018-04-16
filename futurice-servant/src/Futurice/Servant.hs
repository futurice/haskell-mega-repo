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
    serverService,
    serverDescription,
    serverApp,
    serverMiddleware,
    serverColour,
    serverEnvPfx,
    serverOpts,
    -- * Options
    optionsFlag,
    -- ** WAI
    Application,
    Middleware,
    -- ** Cache
    Cache,
    newCache,
    cachedIO,
    genCachedIO,
    CachePolicy(..),
    -- ** Message Queue
    MessageQueue,
    Message (..),
    publishMessage,
    forEachMessage,
    -- * Command response
    CommandResponse (..),
    -- * Re-export
    Job,
    Service (..),
    ) where

import Control.Concurrent.STM
       (TVar, atomically, newTVarIO, swapTVar)
import Control.Lens                         (each)
import Control.Monad.Catch                  (fromException, handleAll)
import Data.Aeson                           (FromJSON, ToJSON)
import Data.Char                            (isAscii, isControl)
import Data.Swagger                         hiding (port)
import Data.Text.Encoding                   (decodeLatin1)
import Data.Time                            (addUTCTime)
import Development.GitRev                   (gitCommitDate, gitHash)
import Futurice.Cache
       (Cache, CachePolicy (..), cacheSize, cachedIO, cleanupCache,
       genCachedIO, newCache)
import Futurice.Colour
       (AccentColour (..), AccentFamily (..), Colour (..), SColour)
import Futurice.EnvConfig
       (Configure, configure, envAwsCredentials, envVar, envVarWithDefault,
       getConfig', optionalAlt)
import Futurice.Lucid.Foundation            (VendorAPI, vendorServer)
import Futurice.MessageQueue
       (Message (..), MessageQueue, createMessageQueue, forEachMessage,
       publishMessage)
import Futurice.Metrics.RateMeter           (mark, values)
import Futurice.Periocron
       (Job, defaultOptions, every, mkJob, shifted, spawnPeriocron)
import Futurice.Prelude
import Futurice.Services                    (Service (..), serviceToText)
import Log.Backend.CloudWatchLogs
       (createCloudWatchLogStream, withCloudWatchLogger)
import Network.Wai
       (Middleware, requestHeaders, responseLBS, responseStatus)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Prelude ()
import Servant
import Servant.CSV.Cassava                  (CSV)
import Servant.Futurice.Favicon             (FutuFaviconAPI, serveFutuFavicon)
import Servant.HTML.Lucid                   (HTML)
import Servant.Server.Internal              (passToServer)
import Servant.Swagger
import Servant.Swagger.UI

import qualified Data.Aeson                as Aeson
import qualified Data.Map.Strict           as Map
import qualified FUM.Types.Login           as FUM
import qualified GHC.Stats                 as Stats
import qualified Network.HTTP.Types        as H
import qualified Network.HTTP.Types.Status as HTTP
import qualified Network.Wai.Handler.Warp  as Warp
import qualified Options.Applicative       as O

import qualified Network.AWS                          as AWS
import qualified Network.AWS.CloudWatch.PutMetricData as AWS
import qualified Network.AWS.CloudWatch.Types         as AWS

import Data.Typeable

type FuturiceAPI api colour =
    FutuFaviconAPI colour
    :<|> api
    :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"
    :<|> VendorAPI

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
    -> Cache
    -> Proxy api
    -> Server api
    -> Server (FuturiceAPI api colour)
futuriceServer t d _cache papi server
    = serveFutuFavicon
    :<|> server
    :<|> swaggerSchemaUIServer (swaggerDoc t d papi)
    :<|> vendorServer

-------------------------------------------------------------------------------
-- main boilerplate
-------------------------------------------------------------------------------

-- | Data type containing the server setup
data ServerConfig f g opts (colour :: Colour) ctx api = SC
    { _serverService     :: !(f Service)
    , _serverDescription :: !Text
    , _serverApplication :: ctx -> Server api
    , _serverMiddleware  :: ctx -> Middleware
    , _serverEnvPfx      :: !(g Text)
    , _serverOpts        :: !(O.Parser opts)
    }

-- | Default server config, through the lenses the type of api will be refined
--
emptyServerConfig :: ServerConfig Proxy Proxy () 'FutuGreen ctx (Get '[JSON] ())
emptyServerConfig = SC
    { _serverService      = Proxy
    , _serverDescription  = "Some futurice service"
    , _serverApplication  = \_ -> pure ()
    , _serverMiddleware   = futuriceNoMiddleware
    , _serverEnvPfx       = Proxy
    , _serverOpts         = pure ()
    }

-- | Default middleware: i.e. nothing.
futuriceNoMiddleware :: ctx -> Middleware
futuriceNoMiddleware = liftFuturiceMiddleware id

-- | Lift config-less middleware for use with 'futuriceServerMain'.
liftFuturiceMiddleware :: Middleware -> ctx -> Middleware
liftFuturiceMiddleware mw _ = mw

serverService :: Lens
    (ServerConfig f g opts colour ctx api)
    (ServerConfig I g opts colour ctx api)
    (f Service)
    Service
serverService = lens _serverService $ \sc x -> sc { _serverService = I x }

serverDescription :: Lens' (ServerConfig f g opts colour ctx api) Text
serverDescription = lens _serverDescription $ \sc x -> sc { _serverDescription = x }

serverEnvPfx :: Lens
    (ServerConfig f g opts colour ctx api)
    (ServerConfig f I opts colour ctx api)
    (g Text)
    Text
serverEnvPfx = lens _serverEnvPfx $ \sc x -> sc { _serverEnvPfx = I x }

serverApp
    :: Functor ff
    => Proxy api'
    -> LensLike ff (ServerConfig f g opts colour ctx api) (ServerConfig f g opts colour ctx api')
       (ctx -> Server api) (ctx -> Server api')
serverApp _ = lens _serverApplication $ \sc x -> sc { _serverApplication = x }

serverMiddleware :: Lens' (ServerConfig f g opts colour ctx api) (ctx -> Middleware)
serverMiddleware = lens _serverMiddleware $ \sc x -> sc { _serverMiddleware = x }

serverColour
    :: Lens (ServerConfig f g opts colour ctx api) (ServerConfig f g opts colour' ctx api)
       (Proxy colour) (Proxy colour')
serverColour = lens (const Proxy) $ \sc _ -> coerce sc

serverOpts
    :: Lens (ServerConfig f g opts colour ctx api) (ServerConfig f g opts' colour ctx api)
       (O.Parser opts) (O.Parser opts')
serverOpts = lens _serverOpts $ \sc x -> sc { _serverOpts = x }

args :: O.Parser opts -> O.Parser (Bool, Middleware, opts)
args o = (,,)
    <$> (printEnv <|> pure False)
    <*> (middleware <|> pure id)
    <*> o
  where
    printEnv = O.flag' True $ mconcat
        [ O.long "help-env-config"
        , O.help "Show env-config help, and quit"
        ]

    middleware = O.flag' (logStdoutDev . m) $ mconcat
        [ O.long "middleware-log-stdout"
        , O.help "Add development request logger middleware."
        ]

    m app req res = do
        print req
        app req res

futuriceServerMain
    :: forall cfg opts ctx api colour.
       (Configure cfg, HasSwagger api, HasServer api '[], SColour colour)
    => (opts -> cfg -> Logger -> Manager -> Cache -> MessageQueue -> IO (ctx, [Job]))
       -- ^ Initialise the context for application, add periocron jobs
    -> ServerConfig I I opts colour ctx api
       -- ^ Server configuration
    -> IO ()
futuriceServerMain makeCtx (SC (I service) d server middleware1 (I envpfx) optsP) = do
    (_showEnvConvig, middleware2, opts) <- O.execParser $ O.info (O.helper <*> args optsP) $ mconcat
        [ O.fullDesc
        , O.progDesc (d ^. unpacked)
        ]
    main1 (\ctx -> middleware1 ctx . middleware2) opts
  where
    main1 :: (ctx -> Middleware) -> opts -> IO ()
    main1 middleware opts = withStderrLogger $ \logger ->
        handleAll (handler logger) $ do
            let t = serviceToText service

            cfg <- runLogT "futurice-servant" logger $ do
                logInfo_ $ "Hello, " <> t <> " is alive"
                getConfigWithPorts (envpfx ^. from packed)

            let awsGroup = fromMaybe "Haskell" (_cfgCloudWatchGroup cfg )

            menv <- for (_cfgCloudWatchCreds cfg) $ \awsCreds -> do
                env' <- AWS.newEnv awsCreds
                return $ env'
                    & AWS.envRegion .~ AWS.Frankfurt  -- TODO: make configurable?

            let main' = main2 middleware opts cfg menv

            case menv of
                Nothing -> main' logger
                Just env -> do
                    createCloudWatchLogStream env awsGroup t
                    withCloudWatchLogger env awsGroup t $ \leLogger -> main' $
                        if fromMaybe True (_cfgStderrLogger cfg)
                        then (logger <> leLogger)
                        else leLogger

    main2 :: (ctx -> Middleware) -> opts -> Cfg cfg -> Maybe AWS.Env -> Logger -> IO ()
    main2 middleware opts (Cfg cfg p _ekgP mgroup _ _) menv lgr = do
        let awsGroup   = fromMaybe "Haskell" mgroup

        -- create message queue and
        -- immediately send that service is starting
        mq <- createMessageQueue lgr menv awsGroup service
        publishMessage mq (ServiceStarting service)

        mgr            <- newManager tlsManagerSettings
        cache          <- newCache
        (ctx, jobs)    <- makeCtx opts cfg lgr mgr cache mq
        let server'    =  futuriceServer (serviceToText service) d cache proxyApi (server ctx)
                       :: Server (FuturiceAPI api colour)

        statsEnabled <-
#if MIN_VERSION_base(4,10,0)
            Stats.getRTSStatsEnabled
#else
            Stats.getGCStatsEnabled
#endif
        mutgcTVar <- newTVarIO (MutGC 0 0 0 0)

        let mcloudwatchJob = do
                guard statsEnabled
                env <- menv
                pure (cloudwatchJob cache mutgcTVar lgr env awsGroup service)

        let jobs' = mkJob "dynmap-cache-cleanup" (cacheCleanupJob cache lgr) (shifted 5 $ every (15 * 60))
                  : maybeToList (mkJob "cloudwatch" <$> mcloudwatchJob <*> pure (every 60))
                  ++ jobs
        _ <- spawnPeriocron (defaultOptions lgr) jobs'

        runLogT "futurice-servant" lgr $ do
            logInfo_ $ "Starting " <> serviceToText service <> " at port " <> textShow p
            logInfo_ $ "-          http://localhost:" <> textShow p <> "/"
            logInfo_ $ "- swagger: http://localhost:" <> textShow p <> "/swagger-ui/"
            -- logInfo_ $ "- ekg:     http://localhost:" <> textShow ekgP <> "/"

        Warp.runSettings (settings p lgr)
            $ middleware ctx
            $ waiMiddleware
            $ serve proxyApi' server'

    cacheCleanupJob :: Cache -> Logger -> IO ()
    cacheCleanupJob cache logger = runLogT "cache-cleanup" logger $ do
        now <- currentTime
        -- Cutoff one hour in the past.
        -- so the items with expiration moment over an hour ago will be removed.
        -- We don't use current moment, to make 'ReturnOld' policy items work
        let stamp = addUTCTime (-3600) now

        sizeBefore <- cacheSize cache
        cleanupCache cache stamp
        sizeAfter <- cacheSize cache

        logInfo "DynMap cache cleaned up" $ Aeson.object
            [ "size-before" Aeson..= sizeBefore
            , "size-after"  Aeson..= sizeAfter
            , "now"         Aeson..= now
            , "stamp"       Aeson..= stamp
            ]



    waiMiddleware :: Middleware
    waiMiddleware app req res = do
        mark "WAI: incoming request"
        app req $ \r -> do
            let code = HTTP.statusCode (responseStatus r)
            mark $ "WAI: response " <> textShow (code `div` 100) <> "xx"
            res r

    handler logger e = do
        runLogT "futurice-servant" logger $ logAttention_ $ textShow e
        throwM e

    settings p logger = Warp.defaultSettings
        & Warp.setPort p
        & Warp.setOnException (onException logger)
        & Warp.setOnExceptionResponse onExceptionResponse
        & Warp.setServerName (encodeUtf8 (serviceToText service))

    onException logger mreq e = do
        print $ typeOf e
        runLogT "warp" logger $ do
            let te = textShow e
            case mreq of
                -- if there isn't request, only warn.
                Nothing -> logInfo_ te
                Just req  -> do
                    liftIO $ mark "Warp caught exception"
                    logAttention te req

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
-- Cloudwatch Job
-------------------------------------------------------------------------------

cloudwatchJob :: Cache -> TVar MutGC -> Logger -> AWS.Env -> Text -> Service -> IO ()
cloudwatchJob cache mutgcTVar logger env awsGroup service = runLogT "cloudwatch" logger $ do
    let awsService = serviceToText service

    -- averages
    meters <- liftIO values
    logInfo "Futurice.Metrics" meters
    let mkDatum (n, v) = AWS.metricDatum ("Count: " <> n)
            & AWS.mdValue      ?~ fromIntegral v
            & AWS.mdUnit       ?~ AWS.Count
            & AWS.mdDimensions .~ [AWS.dimension "Service" awsService]
    let meterDatums = map mkDatum (Map.toList meters)

    -- Cache size
    cs <- cacheSize cache
    let cacheDatum = AWS.metricDatum "Gauge: Cache size"
            & AWS.mdValue      ?~ fromIntegral cs
            & AWS.mdUnit       ?~ AWS.Count
            & AWS.mdDimensions .~ [AWS.dimension "Service" awsService]

    -- gcm
#if MIN_VERSION_base(4,10,0)
    stats <- liftIO Stats.getRTSStats

    let liveBytes =  Stats.gcdetails_live_bytes (Stats.gc stats)

    let currMut = Stats.mutator_cpu_ns stats
    let currTot = Stats.cpu_ns stats

    let currWMut = Stats.mutator_elapsed_ns stats
    let currWTot = Stats.elapsed_ns stats
#else
    stats <- liftIO Stats.getGCStats

    let liveBytes = Stats.currentBytesUsed stats

    let currMut = Stats.mutatorCpuSeconds stats
    let currTot = Stats.cpuSeconds stats

    let currWMut = Stats.mutatorWallSeconds stats
    let currWTot = Stats.wallSeconds stats
#endif

    MutGC prevMut prevTot prevWMut prevWTot <- liftIO $ atomically $
        swapTVar mutgcTVar (MutGC currMut currTot currWMut currWTot)

    let mutSec  = currMut - prevMut
    let totSec  = currTot - prevTot

    let mutWSec = currWMut - prevWMut
    let totWSec = currWTot - prevWTot

    let clampPercentage x
          | x < 0     = 0
          | x > 1     = 100
          | otherwise = 100 * x

    let productivity = clampPercentage (realToFrac mutSec  / realToFrac totSec  :: Double)
    let prodWall     = clampPercentage (realToFrac mutWSec / realToFrac totWSec :: Double)

    logInfo "RTS stats" $ Aeson.object
        [ "live bytes"         Aeson..= liveBytes
        , "productivity cpu"   Aeson..= productivity
        , "productivity wall"  Aeson..= prodWall
        , "cache size"         Aeson..= cs
        ]

    rs <- liftIO $ AWS.runResourceT $ AWS.runAWS env $ do

        -- Residency
        let datum1 = AWS.metricDatum "Live bytes"
                & AWS.mdValue      ?~ fromIntegral liveBytes
                & AWS.mdUnit       ?~ AWS.Bytes
                & AWS.mdDimensions .~ [AWS.dimension "Service" awsService]
        -- Productivity
        let datum2 = AWS.metricDatum "Productivity"
                & AWS.mdValue      ?~ productivity
                & AWS.mdUnit       ?~ AWS.Percent
                & AWS.mdDimensions .~ [AWS.dimension "Service" awsService]
        -- Productivity Wall
        let datum3 = AWS.metricDatum "Productivity Wall"
                & AWS.mdValue      ?~ prodWall
                & AWS.mdUnit       ?~ AWS.Percent
                & AWS.mdDimensions .~ [AWS.dimension "Service" awsService]

        -- Put.
        let datums' = datum1 : datum2 : datum3
                   : cacheDatum : meterDatums
        -- metric names can be only ASCII
        let datums = datums'
                & traverse . AWS.mdMetricName . each %~ makeAscii
        let pmd = AWS.putMetricData (awsGroup <> "/RTS")
                & AWS.pmdMetricData .~ datums

        AWS.send pmd

    logInfo_ $ "cloudwatch response " <> textShow rs

-------------------------------------------------------------------------------
-- MutGC
-------------------------------------------------------------------------------

#if MIN_VERSION_base(4,10,0)
data MutGC = MutGC !Stats.RtsTime !Stats.RtsTime !Stats.RtsTime !Stats.RtsTime
#else
data MutGC = MutGC !Double !Double !Double !Double
#endif

-------------------------------------------------------------------------------
-- SSO User
-------------------------------------------------------------------------------

data SSOUser

instance HasServer api context => HasServer (SSOUser :> api) context where
    type ServerT (SSOUser :> api) m = Maybe FUM.Login -> ServerT api m

    hoistServerWithContext _ pc nt s =
        hoistServerWithContext (Proxy :: Proxy api) pc nt . s

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
-- Options applicative
-------------------------------------------------------------------------------

optionsFlag
    :: a              -- ^ default value
    -> [(a, String)]  -- ^ values
    -> String         -- ^ help text
    -> O.Parser a
optionsFlag def ys h = start ys
  where
    start []            = pure def
    start ((x, l) : xs) = O.flag' x (O.long l <> O.help h) <|> go xs
    go []               = pure def
    go ((x, l) : xs)    = O.flag' x (O.long l) <|> go xs

-------------------------------------------------------------------------------
-- Config
-------------------------------------------------------------------------------

data Cfg cfg = Cfg
    { _cfgInner           :: !cfg
    , _cfgPort            :: !Int
    , _cfgEkgPort         :: !Int
    , _cfgCloudWatchGroup :: !(Maybe Text)
    , _cfgCloudWatchCreds :: !(Maybe AWS.Credentials)
    , _cfgStderrLogger    :: !(Maybe Bool)
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
    <*> optionalAlt (envVar "USE_STDERR_LOGGER")

-------------------------------------------------------------------------------
-- Defaults
-------------------------------------------------------------------------------

defaultPort :: Int
defaultPort = 8000

defaultEkgPort :: Int
defaultEkgPort = 9000

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

makeAscii :: Char -> Char
makeAscii c
    | isAscii c && not (isControl c) = c
    | otherwise                      = '?'

-------------------------------------------------------------------------------
-- CommandResponse
-------------------------------------------------------------------------------

data CommandResponse a
    = CommandResponseOk a            -- ^ Do nothing
    | CommandResponseError String    -- ^ an error
    | CommandResponseReload          -- ^ reload current page
    | CommandResponseRedirect !Text  -- ^ redirect to the url
  deriving (Eq, Ord, Show, Typeable, Generic, Functor, Foldable, Traversable)

instance ToJSON a => ToJSON (CommandResponse a)
instance FromJSON a => FromJSON (CommandResponse a)
instance ToSchema a => ToSchema (CommandResponse a) where
    declareNamedSchema = Data.Swagger.genericDeclareNamedSchemaUnrestricted Data.Swagger.defaultSchemaOptions
