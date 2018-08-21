{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Croned where

import Control.Concurrent.STM
import Data.Machine
import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Lucid.Navigation
import Futurice.Servant
import Futurice.Prelude
import Control.Monad.Catch (bracket)
import Prelude ()
import Servant.API
import Servant.API.Generic
import Servant.HTML.Lucid        (HTML)
import Servant.Server
import Servant.Server.Generic
import System.Exit               (ExitCode (..))
import System.Environment (getArgs)
import System.IO                 (Handle, hIsEOF)

import qualified Control.Concurrent.Async as A
import qualified Data.ByteString          as BS
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Process           as Proc

-------------------------------------------------------------------------------
-- main
-------------------------------------------------------------------------------

defaultMain :: IO ()
defaultMain = withStderrLogger $ \lgr -> do
    args <- getArgs
    -- TODO: read ENV for period and run periocron
    ctx <- atomically $ Ctx lgr args
        <$> newTVar False
        <*> newTVar Nothing
        <*> newTVar []
        <*> newTVar []

    putStrLn "Staring croned-server at http://localhost:8000"
    Warp.runSettings settings $ serve proxyApi $ server ctx
  where
    settings = Warp.defaultSettings
        & Warp.setPort 8000

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

        (ec, ()) <- liftIO $ readProcessMachines "sh" ["slow.sh"] Nothing $ \mout merr ->
            A.runConcurrently $
                A.Concurrently (drain (ctxStdout ctx) mout) *>
                A.Concurrently (drain (ctxStderr ctx) merr)

        logInfo_ $ ("Completed... " ++ show ec) ^. packed
        liftIO $ atomically $ writeTVar (ctxLastRun ctx) $ Just $ LastRun
            { lrExitCode = ec
            , lrStarted  = now
            }

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
            dd_ $ pre_ $ toHtml $ show $ ctxArgs ctx

        when l $ h2_ "Running"

        for_ mlr $ \lr -> do
            h2_ "Last run"
            dl_ $ do
                dt_ "Started "
                dd_ $ toHtml $ show $ lrStarted lr
                dt_ "ExitCode "
                dd_ $ toHtml $ show $ lrExitCode lr

        h2_ "Stdout"
        pre_ $ toHtml $ decodeUtf8Lenient $ mconcat $ reverse sout

        h2_ "Stderr"
        pre_ $ toHtml $ decodeUtf8Lenient $ mconcat $ reverse serr

-------------------------------------------------------------------------------
-- Trigger Handler
-------------------------------------------------------------------------------

triggerHandler :: Ctx -> Handler Text
triggerHandler ctx = do
    wasRunning <- liftIO $ trigger ctx
    if wasRunning then return "Already running" else return "Triggered"

-------------------------------------------------------------------------------
-- API
-------------------------------------------------------------------------------

data Routes route = Routes
    { routeIndex   :: route :- Get '[HTML] (HtmlPage "index")
    , routeTrigger :: route :- "trigger" :> Post '[JSON] Text
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
    { lrStarted :: UTCTime
    , lrExitCode :: ExitCode
    }

-------------------------------------------------------------------------------
-- "machines-process"
-------------------------------------------------------------------------------

-- | Variant of 'readProcessWithExitCode'
readProcessMachines :: FilePath                                                              -- ^ Filename of the executable
                    -> [String]                                                              -- ^ any arguments
                    -> Maybe (MachineT IO k BS.ByteString)                                   -- ^ standard input
                    -> (MachineT IO k BS.ByteString -> MachineT IO k BS.ByteString -> IO c)  -- ^ continuation given stdout and stderr
                    -> IO (ExitCode, c)
readProcessMachines cmd args mstdin kont =
    Proc.withCreateProcess cp $ \(Just inh) (Just outh) (Just errh) ph -> do
        let mstdout = sourceHandleWith (flip BS.hGetSome 4096) outh
        let mstderr = sourceHandleWith (flip BS.hGetSome 4096) errh
        let inputAction = for_ mstdin $ \m -> runT_ $
              repeatedly (await >>= lift . BS.hPut inh) <~ m

        A.runConcurrently $ (,)
            <$ A.Concurrently inputAction
            <*> A.Concurrently (Proc.waitForProcess ph)
            <*> A.Concurrently (kont mstdout mstderr)
  where
    cp0 = Proc.proc cmd args
    cp = cp0
        { Proc.std_in  = Proc.CreatePipe
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
