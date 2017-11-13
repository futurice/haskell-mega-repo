{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}
-- |  The code in this module is a cut down version of
-- <http://hackage.haskell.org/package/warp warp>, which is distributed under MIT license.
--
-- About name: Arcola is a sublabel of Warp. See <https://en.wikipedia.org/wiki/Arcola Wikipedia page>.
-- There's <https://www.youtube.com/watch?v=XqwuBgT0GA4 some music> on YouTube too.
--
module Network.Arcola (
    -- * Application
    Application,
    lazyIOApplication,
    -- * Running
    run,
    runSettings,
    Port,
    -- * Settings
    Settings (..),
    defaultSettings,
    -- * Connection
    Connection (..),
    ) where

import Control.Concurrent              (forkIOWithUnmask)
import Control.Exception
       (SomeException (..), allowInterrupt, bracket, catch, displayException,
       finally, handle, mask_, toException, try)
import Control.Monad                   (when)
import Data.ByteString                 (ByteString)
import Data.Foldable                   (traverse_)
import Data.Functor                    (void)
import Data.Streaming.Network          (bindPortTCP)
import Data.Streaming.Network.Internal (HostPreference (HostIPv4))
import Foreign.C.Error                 (Errno (..), eCONNABORTED)
import GHC.IO.Exception                (ioe_errno)
import Network.Socket
       (SockAddr, Socket, SocketOption (NoDelay), accept, close,
       setSocketOption, withSocketsDo)
import Network.Socket.ByteString       (recv, sendAll)
import System.IO                       (hPutStrLn, stderr)

-- Used to implement lazyIOApplication
import System.IO.Unsafe (unsafeInterleaveIO)

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Lazy.Internal as LBSI

type Port = Int

-------------------------------------------------------------------------------
-- Application
-------------------------------------------------------------------------------

-- | An application is an action on a connection.
type Application = Connection -> IO ()


-- | Converts a lazy 'LBS.ByteString' endomorphism into an 'Application'.
lazyIOApplication :: (LBS.ByteString -> LBS.ByteString) -> Application
lazyIOApplication f conn = do
    lbs <- lazyRead
    traverse_ (connSendAll conn) (LBS.toChunks (f lbs))
  where
    lazyRead = unsafeInterleaveIO loop
    loop = do
        c <- connRecv conn -- only blocks if there is no data available
        if BS.null c
        then connClose conn >> return LBSI.Empty
        else do
            cs <- lazyRead
            return (LBSI.Chunk c cs)

-------------------------------------------------------------------------------
-- Run
-------------------------------------------------------------------------------

-- | Run 'Application' with 'defaultSettings'.
run :: Port -> Application -> IO ()
run p = runSettings defaultSettings { settingsPort = p }

-- | Run 'Application' with 'Settings'.
runSettings :: Settings -> Application -> IO ()
runSettings set app = withSocketsDo $ bracket
    (bindPortTCP (settingsPort set) (settingsHost set))
    close
    (\socket -> runSettingsSocket set socket app)

-------------------------------------------------------------------------------
-- Settings
-------------------------------------------------------------------------------

-- | Various Arcola settings.
--
-- TODO: make abstract, add lenses.
data Settings = Settings
    { settingsPort :: Port -- ^ Port to listen on. Default value: 3000
    , settingsHost :: HostPreference -- ^ Default value: HostIPv4
    , settingsOnException :: SomeException -> IO () -- ^ What to do with exceptions thrown by either the application or server. Default: ignore server-generated exceptions (see 'InvalidRequest') and print application-generated applications to stderr.
    , settingsOnOpen :: SockAddr -> IO Bool -- ^ What to do when a connection is open. When 'False' is returned, the connection is closed immediately. Otherwise, the connection is going on. Default: always returns 'True'.
    , settingsOnClose :: SockAddr -> IO ()  -- ^ What to do when a connection is close. Default: do nothing.
    , settingsBeforeMainLoop :: IO ()
      -- ^ Code to run after the listening socket is ready but before entering
      -- the main event loop. Useful for signaling to tests that they can start
      -- running, or to drop permissions after binding to a restricted port.
      --
      -- Default: do nothing.

    , settingsFork :: ((forall a. IO a -> IO a) -> IO ()) -> IO ()
      -- ^ Code to fork a new thread to accept a connection.
      --
      -- This may be useful if you need OS bound threads, or if
      -- you wish to develop an alternative threading model.
      --
      -- Default: @void . forkIOWithUnmask@
    }

-- |  The default settings for the Arcola server.
defaultSettings :: Settings
defaultSettings = Settings
    { settingsPort = 9999
    , settingsHost = HostIPv4
    , settingsOnException = hPutStrLn stderr . displayException
    , settingsOnOpen = \_ -> return True
    , settingsOnClose = \_ -> return ()
    , settingsBeforeMainLoop = return ()
    , settingsFork = defaultSettingsFork
    }
  where
    defaultSettingsFork :: ((forall a. IO a -> IO a) -> IO ()) -> IO ()
    defaultSettingsFork f = void (forkIOWithUnmask f)

-------------------------------------------------------------------------------
-- Connection
-------------------------------------------------------------------------------

-- | A very thin abstraction over a socket.
data Connection = Connection
    { connSendAll     :: ByteString -> IO ()
    -- ^ The sending function.
    , connClose       :: IO ()
    -- ^ The connection closing function. Warp guarantees it will only be
    -- called once. Other functions (like 'connRecv') may be called after
    -- 'connClose' is called.
    , connRecv        :: IO ByteString
    -- ^ The connection receiving function. This returns "" for EOF.
    }

socketConnection :: Socket -> Connection
socketConnection s = Connection
    { connRecv    = recv s 4096
    , connSendAll = sendAll s
    , connClose   = close s
    }

-------------------------------------------------------------------------------
-- Low-level
-------------------------------------------------------------------------------

runSettingsSocket :: Settings -> Socket -> Application -> IO ()
runSettingsSocket set socket app = do
    runSettingsConnection set getConn app
  where
    getConn = do
#if WINDOWS
        (s, sa) <- windowsThreadBlockHack $ accept socket
#else
        (s, sa) <- accept socket
#endif
        -- NoDelay causes an error for AF_UNIX.
        setSocketOption s NoDelay 1 `catch` \(SomeException _) -> return ()
        return (socketConnection s, sa)

    closeListenSocket = close socket

runSettingsConnection :: Settings -> IO (Connection, SockAddr) -> Application -> IO ()
runSettingsConnection set getConn app = do
    settingsBeforeMainLoop set
    void $ mask_ acceptLoop
  where
    acceptLoop = do
        -- Allow async exceptions before receiving the next connection maker.
        allowInterrupt

        -- In contrast to warp, we create 'Connection' in the loop
        -- as our 'Connection's are cheap to create.
        mx <- acceptNewConnection
        case mx of
            Nothing          -> return ()
            Just (conn, addr) -> do
                fork set conn addr app
                acceptLoop

    acceptNewConnection = do
        ex <- try getConn
        case ex of
            Right x -> return (Just x)
            Left e -> do
                let eConnAborted = getErrno eCONNABORTED
                    getErrno (Errno cInt) = cInt
                if ioe_errno e == Just eConnAborted
                    then acceptNewConnection
                    else do
                        settingsOnException set $ toException e
                        return Nothing

fork :: Settings -> Connection -> SockAddr -> Application -> IO ()
fork set conn addr app = settingsFork set $ \unmask ->
    -- Call the user-supplied on exception code if any
    -- exceptions are thrown.
    handle (settingsOnException set) $ finally (serve unmask) (connClose conn)
  where
    -- We need to register a timeout handler for this thread, and
    -- cancel that handler as soon as we exit. We additionally close
    -- the connection immediately in case the child thread catches the
    -- async exception or performs some long-running cleanup action.
    serve unmask = unmask $ do
       -- Call the user-supplied code for connection open and
       -- close events
       bracket onOpen onClose $ \goingon ->
           -- Actually serve this connection.  bracket with closeConn
           -- above ensures the connection is closed.
           when goingon $ serveConnection conn addr set app

    onOpen = settingsOnOpen  set addr
    onClose _ = settingsOnClose set addr

serveConnection :: Connection -> SockAddr -> Settings -> Application -> IO ()
serveConnection conn _addr _set app = app conn
