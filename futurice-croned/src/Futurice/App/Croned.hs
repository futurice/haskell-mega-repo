{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Croned where

import Control.Applicative       ((<**>))
import Control.Concurrent        (forkIO)
import Control.Concurrent.STM
import Control.Monad.Catch       (bracket)
import Data.Aeson                (ToJSON)
import Data.Machine
import Data.Swagger              (ToSchema)
import Futurice.Clock
import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Lucid.Navigation
import Futurice.Periocron        (defaultOptions, every, mkJob, spawnPeriocron)
import Futurice.Prelude
import Futurice.Servant
import Prelude ()
import Servant.API
import Servant.API.Generic
import Servant.HTML.Lucid        (HTML)
import Servant.Server
import Servant.Server.Generic
import System.Exit               (ExitCode (..))
import System.IO                 (Handle, hIsEOF)

import qualified Control.Concurrent.Async as A
import qualified Data.ByteString          as BS
import qualified Network.Wai.Handler.Warp as Warp
import qualified Options.Applicative      as O
import qualified System.Process           as Proc

-------------------------------------------------------------------------------
-- main
-------------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = withStderrLogger $ \lgr -> do
    Opts {..} <- O.execParser opts

    ctx <- atomically $ Ctx lgr optsCommand optsArgs
        <$> newTVar False
        <*> newTVar Nothing
        <*> newTVar []
        <*> newTVar []

    _ <- spawnPeriocron (defaultOptions lgr)
        [ mkJob "Script" (trigger ctx) (every $ 60 * fromIntegral optsInterval)
        ]

    putStrLn "Staring croned-server at http://localhost:8000"
    Warp.runSettings (settings optsPort) $ serve proxyApi $ server ctx
  where
    settings port = Warp.defaultSettings
        & Warp.setPort port

    opts = O.info (optsP <**> O.helper) $ mconcat
        [ O.fullDesc
        , O.progDesc "Wrap script in a service"
        , O.header "croned-server - run script as a background task"
        ]

-------------------------------------------------------------------------------
-- cli options
-------------------------------------------------------------------------------

data Opts = Opts
    { optsPort     :: Int
    , optsInterval :: Int
    , optsCommand  :: String
    , optsArgs     :: [String]
    }

optsP :: O.Parser Opts
optsP = Opts <$> portP <*> intervalP <*> commandArg <*> args where
    portP = O.option (clamped 1 0xffff) $ mconcat
        [ O.short 'p'
        , O.long "port"
        , O.metavar "PORT"
        , O.help "Port service will run with"
        , O.value 8000
        , O.showDefault
        ]

    intervalP = O.option (clamped 1 0xffff) $ mconcat
        [ O.short 'p'
        , O.long "interval"
        , O.metavar "MINUTES"
        , O.help "interval script will run (in minutes)"
        , O.value 60
        , O.showDefault
        ]

    commandArg = O.strArgument $ mconcat
        [ O.metavar "CMD"
        ]
    args = many $ O.strArgument $ mconcat
        [ O.metavar "ARGS..."
        ]

    clamped :: Int -> Int -> O.ReadM Int
    clamped mi ma = O.eitherReader $ \s -> case readMaybe s of
        Nothing         -> Left "Int expected"
        Just x | x < mi -> Left $ show x ++ " < " ++ show mi
        Just x | x > ma -> Left $ show x ++ " > " ++ show mi
        Just x          -> Right x

-------------------------------------------------------------------------------
-- Trigger
-------------------------------------------------------------------------------

trigger :: Ctx -> IO Bool
trigger ctx = runLogT "trigger" (ctxLogger ctx) $ do
    logInfo_ "Trying to trigger"
    bracket ack rel action
  where
    ack :: LogT IO Bool
    ack = lift $ atomically $ do
        l <- readTVar (ctxLock ctx)
        unless l $ do
            writeTVar (ctxLock ctx)    True
            writeTVar (ctxLastRun ctx) Nothing
            writeTVar (ctxStdout ctx)  []
            writeTVar (ctxStderr ctx)  []
        return l

    rel :: Bool -> LogT IO ()
    rel l = unless l $ liftIO $ atomically $ writeTVar (ctxLock ctx) False

    action :: Bool -> LogT IO Bool
    action l = unless l go >> return l

    go :: LogT IO ()
    go = do
        logInfo_ "Starting..."
        now <- currentTime

        (ts, (ec, ())) <- clocked $ liftIO $
            readProcessMachines (ctxCmd ctx) (ctxArgs ctx) $ \mout merr ->
                A.runConcurrently $
                    A.Concurrently (drain (ctxStdout ctx) mout) *>
                    A.Concurrently (drain (ctxStderr ctx) merr)

        logInfo_ $ ("Completed... " ++ show ec) ^. packed
        (out, err) <- liftIO $ atomically $ do
            writeTVar (ctxLastRun ctx) $ Just $ LastRun
                { lrExitCode = ec
                , lrRuntime  = ts
                , lrStarted  = now
                }
            
            out <- readText ctxStdout
            err <- readText ctxStdout
            return (out, err)

        logInfo "Stdout was" out
        logInfo "Stdout was" err

    readText f = decodeUtf8Lenient . mconcat . reverse <$> readTVar (f ctx)

    drain :: TVar [BS.ByteString] -> MachineT IO k ByteString -> IO ()
    drain tvar m = runT_ $ sink <~ m where
        sink = repeatedly $ do
            bs <- await
            lift $ atomically $ modifyTVar' tvar (bs :)

-------------------------------------------------------------------------------
-- Server
-------------------------------------------------------------------------------

server :: Ctx -> Server Api
server ctx = futuriceServer "Croned" "Croned runner" (error "no cache")
    (genericApi (Proxy :: Proxy Routes))
    $ genericServer $ Routes
        { routeIndex   = indexPage ctx
        , routeTrigger = triggerHandler ctx
        , routeOutputs = outputsHandler ctx
        }

-------------------------------------------------------------------------------
-- Navigation
-------------------------------------------------------------------------------

data Nav
    = NavHome
  deriving (Eq, Ord, Enum, Bounded)

instance Navigation Nav where
    serviceTitle _ = "Croned"

    navLink NavHome = (recordHref_ routeIndex, "Croned")

    pageParams = pageParamsWithJS
        $(makeRelativeToProject "croned.js" >>= embedJS)

-------------------------------------------------------------------------------
-- IndexPage
-------------------------------------------------------------------------------

indexPage :: Ctx -> Handler (HtmlPage "index")
indexPage ctx = liftIO $ do
    (l, mlr, sout, serr) <- atomically $ (,,,)
        <$> readTVar (ctxLock ctx)
        <*> readTVar (ctxLastRun ctx)
        <*> readTVar (ctxStdout ctx)
        <*> readTVar (ctxStderr ctx)
    return $ page_ "Croned" (Just NavHome) $ do
        h2_ "Configuration"
        dl_ $ do
            dt_ "Command"
            dd_ $ pre_ $ toHtml $ show $ ctxCmd ctx : ctxArgs ctx

        when l $ h2_ "Running"
        unless l $ do
            h2_ "Trigger command"
            button_
                [ id_ "futu-trigger"
                , class_ "button primary"
                ]
                "Trigger"

        for_ mlr $ \lr -> do
            h2_ "Last run"
            dl_ $ do
                dt_ "Started "
                dd_ $ toHtml $ show $ lrStarted lr
                dt_ "Runtime"
                dd_ $ toHtml $ show $ timeSpecToSecondsD $ lrRuntime lr
                dt_ "ExitCode "
                dd_ $ toHtml $ show $ lrExitCode lr

        h2_ "Stdout"
        pre_ [ id_ "futu-stdout" ] $ toHtml $ decodeUtf8Lenient $ mconcat $ reverse sout

        h2_ "Stderr"
        pre_ [ id_ "futu-stderr" ] $ toHtml $ decodeUtf8Lenient $ mconcat $ reverse serr

-------------------------------------------------------------------------------
-- Trigger Handler
-------------------------------------------------------------------------------

triggerHandler :: Ctx -> Handler ()
triggerHandler ctx = liftIO $ void $ forkIO $ void $ trigger ctx

-------------------------------------------------------------------------------
-- Output handler
-------------------------------------------------------------------------------

data Outputs = Outputs
    { outStdout :: !Text
    , outStderr :: !Text
    }
  deriving (Generic)

instance ToJSON Outputs
instance ToSchema Outputs

outputsHandler :: Ctx -> Handler Outputs
outputsHandler ctx = liftIO $ atomically $ Outputs
    <$> readText ctxStdout
    <*> readText ctxStderr
  where
    readText f = decodeUtf8Lenient . mconcat . reverse <$> readTVar (f ctx)

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

data Routes route = Routes
    { routeIndex   :: route :- Get '[HTML] (HtmlPage "index")
    , routeTrigger :: route :- "trigger" :> Post '[JSON] ()
    , routeOutputs :: route :- "outputs" :> Get '[JSON] Outputs
    --
    }
  deriving Generic

type Api = FuturiceAPI (ToServantApi Routes) 'FutuGreen

proxyApi :: Proxy Api
proxyApi = Proxy

-------------------------------------------------------------------------------
-- Context
-------------------------------------------------------------------------------

data Ctx = Ctx
    { ctxLogger  :: Logger
    , ctxCmd     :: String
    , ctxArgs    :: [String]
    , ctxLock    :: TVar Bool
    , ctxLastRun :: TVar (Maybe LastRun)
    , ctxStdout  :: TVar [BS.ByteString]
    , ctxStderr  :: TVar [BS.ByteString]
    }

-------------------------------------------------------------------------------
-- LastRun
-------------------------------------------------------------------------------

data LastRun = LastRun
    { lrStarted  :: !UTCTime
    , lrRuntime  :: !TimeSpec
    , lrExitCode :: !ExitCode
    }

-------------------------------------------------------------------------------
-- "machines-process"
-------------------------------------------------------------------------------

-- | Variant of 'readProcessWithExitCode'
readProcessMachines :: FilePath                                                              -- ^ Filename of the executable
                    -> [String]                                                              -- ^ any arguments
                    -> (MachineT IO k BS.ByteString -> MachineT IO k BS.ByteString -> IO c)  -- ^ continuation given stdout and stderr
                    -> IO (ExitCode, c)
readProcessMachines cmd args kont =
    Proc.withCreateProcess cp $ \Nothing (Just outh) (Just errh) ph -> do
        let mstdout = sourceHandleWith (flip BS.hGetSome 4096) outh
        let mstderr = sourceHandleWith (flip BS.hGetSome 4096) errh

        A.runConcurrently $ (,)
            <$> A.Concurrently (Proc.waitForProcess ph)
            <*> A.Concurrently (kont mstdout mstderr)
  where
    cp0 = Proc.proc cmd args
    cp = cp0
        { Proc.std_in  = Proc.NoStream
        , Proc.std_out = Proc.CreatePipe
        , Proc.std_err = Proc.CreatePipe
        }

    sourceHandleWith :: (Handle -> IO a) -> Handle -> MachineT IO k a
    sourceHandleWith f h = sourceIOWith (return h) hIsEOF f

    sourceIOWith :: IO r -> (r -> IO Bool) -> (r -> IO a) -> MachineT IO k a
    sourceIOWith acquire release read' = MachineT $ do
        r         <- acquire
        released  <- release r
        if released then
            return Stop
        else do
            x <- read' r
            return . Yield x $ sourceIOWith acquire release read'
