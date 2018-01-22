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
import Data.Aeson.Compat   (FromJSON (..), decodeStrict, encode, withText)
import Futurice.Prelude
import Prelude ()
import System.Directory    (getAppUserDataDirectory)
import System.Environment  (lookupEnv)
import System.Exit         (exitFailure)
import System.Exit.Lens    (_ExitFailure)
import System.FilePath     ((</>))
import System.IO           (stderr, hPutStr)

import qualified Data.ByteString           as BS
import qualified Data.ByteString.Lazy      as LBS
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

fprArg :: Fingerprint -> String
fprArg (Fingerprint f) = T.unpack f

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

writeStorageAll :: Storage -> Map Text Text -> IO ()
writeStorageAll storage m = do
    let args = "-e" : "-a" : concatMap recipient fprs
    (ec, encrypted, err) <- ProcessBS.readProcessWithExitCode
        "gpg2" args (LBS.toStrict contents)
    forOf_ _ExitFailure ec $ \_ ->  do
        BS.hPutStr stderr err
        exitFailure
    BS.writeFile (dir </> "secrets" </> "all.asc") encrypted
  where
    contents      = encode m
    dir           = storageAppDir storage
    fprs          = storageAllFprs storage
    recipient fpr = ["-r", fprArg fpr ]

writeStorageOwn :: Storage -> Map Text Text -> IO ()
writeStorageOwn storage m = do
    (ec, encrypted, err) <- ProcessBS.readProcessWithExitCode
        "gpg2" ["-e", "-a", "-r", fprArg $ storageFpr storage ] (LBS.toStrict contents)
    forOf_ _ExitFailure ec $ \_ ->  do
        BS.hPutStr stderr err
        exitFailure
    BS.writeFile (dir </> "secrets" </> (T.unpack user ++ ".asc")) encrypted
  where
    contents = encode m
    user     = storageUser storage
    dir      = storageAppDir storage

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
addKey forAll key value = do
    storage <- readStorage
    let m = (if forAll then storageAll else storageOwn) storage
    when (Map.member key m) $ fail $ "key " ++ show key ++ " is already added. Do you want to `update-key`?"

    let m' = m & at key ?~ value
    if forAll
    then writeStorageAll storage m'
    else writeStorageOwn storage m'

updateKey :: Text -> Text -> IO ()
updateKey key value = do
    storage <- readStorage

    if Map.member key (storageOwn storage)
    then updateOwn storage
    else updateAll storage
  where
    updateOwn storage = do
        let m = storageOwn storage
        unless (Map.member key m) $ do
            hPutStr stderr $ "key " ++ show key ++ " doesn't exist."
            exitFailure
        let m' = m & ix key .~ value
        writeStorageOwn storage m'

    updateAll storage = do
        let m = storageAll storage
        unless (Map.member key m) $ do
            hPutStr stderr $ "key " ++ show key ++ " doesn't exist."
            exitFailure
        let m' = m & ix key .~ value
        writeStorageAll storage m'

deleteKey :: Text -> IO ()
deleteKey key = do
    storage <- readStorage

    if Map.member key (storageOwn storage)
    then deleteOwn storage
    else deleteAll storage
  where
    deleteOwn storage = do
        let m = storageOwn storage
        unless (Map.member key m) $ do
            hPutStr stderr $ "key " ++ show key ++ " doesn't exist."
            exitFailure
        let m' = m & at key .~ Nothing
        writeStorageOwn storage m'

    deleteAll storage = do
        let m = storageAll storage
        unless (Map.member key m) $ do
            hPutStr stderr $ "key " ++ show key ++ " doesn't exist."
            exitFailure
        let m' = m & at key .~ Nothing
        writeStorageAll storage m'
