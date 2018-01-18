{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.MegaRepoTool.Keys (
    listKeys,
    addKey,
    updateKey,
    deleteKey,
    -- * low level
    readStorage,
    Storage (..),
    ) where

import Control.Lens        (forOf_)
import Control.Monad.Catch (handleIOError)
import Data.Aeson.Compat   (FromJSON (..), decodeStrict, withText)
import Futurice.Prelude
import Prelude ()
import System.Directory    (getAppUserDataDirectory)
import System.Environment  (lookupEnv)
import System.Exit         (exitFailure)
import System.Exit.Lens    (_ExitFailure)
import System.FilePath     ((</>))
import System.IO           (stderr)

import qualified Data.ByteString as BS

import qualified Data.Map.Strict           as Map
import qualified Data.Text                 as T
import qualified Data.Text.IO              as T
import qualified System.Process.ByteString as ProcessBS

-------------------------------------------------------------------------------
-- Utilitiees
-------------------------------------------------------------------------------

newtype Fingerprint = Fingerprint Text
  deriving (Show)

instance FromJSON Fingerprint where
    parseJSON = withText "Fingerprint" $ pure . Fingerprint . T.replace " " ""

data Storage = Storage
    { storageUser    :: Text
    , storageFpr     :: Fingerprint
    , storageAllFprs :: [Fingerprint]
    , storageAppDir  :: FilePath
    , storageAll     :: Map Text Text
    , storageOwn     :: Map Text Text
    }
  deriving Show

readStorage :: IO Storage
readStorage = do
    user <- lookupEnv "USER" >>= maybe (fail "$USER envvar not set") (return . T.pack)

    dir <- getAppUserDataDirectory "haskell-mega-repo-secrets"

    -- Read user map, i.e. $USER -> key fingerprint
    userMapContents <- BS.readFile $ dir </> "users.json"
    users <- decodeStrict userMapContents :: IO (Map Text Fingerprint)

    fpr <- case Map.lookup user users of
        Just fpr -> return fpr
        Nothing  -> fail $ show user ++ " is not in users.json"

    -- Read contents
    keysAll <- readKeys $ dir </> "secrets" </> "all.asc"
    keysOwn <- readKeys $ dir </> "secrets" </> (T.unpack user ++ ".asc")

    -- Return
    return Storage
        { storageUser    = user
        , storageFpr     = fpr
        , storageAllFprs = toList users
        , storageAppDir  = dir
        , storageAll     = keysAll
        , storageOwn     = keysOwn
        }
  where
    readKeys fp = handleIOError (const mempty) $ do
        encrypted <- BS.readFile fp
        (ec, contents, err) <- ProcessBS.readProcessWithExitCode
            "gpg2" ["-d"] encrypted
        BS.hPutStr stderr err
        forOf_ _ExitFailure ec $ \_ ->  do
            BS.hPutStr stderr err
            exitFailure
        decodeStrict contents

-------------------------------------------------------------------------------
-- Commands
-------------------------------------------------------------------------------

listKeys :: Bool -> IO ()
listKeys _ = do
    storage <- readStorage

    T.putStrLn "=== Shared keys ==="
    ifor_ (storageAll storage) $ \k v ->
        T.putStrLn $ T.justifyLeft 30 ' ' k <> " : " <> v

    T.putStrLn $ "=== " <> storageUser storage <> " keys ==="
    ifor_ (storageOwn storage) $ \k v ->
        T.putStrLn $ T.justifyLeft 30 ' ' k <> " : " <> v

addKey :: Bool -> Text -> Text -> IO ()
addKey _ _ _ = putStrLn "not imlemented"

updateKey :: Text -> Text -> IO ()
updateKey _ _ = putStrLn "not implemented"

deleteKey :: Text -> IO ()
deleteKey _ = putStrLn "not implemented"
