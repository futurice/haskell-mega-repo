{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.MegaRepoTool.BuildDocker (
    cmdBuildDocker,
    cmdBuildCommand,
    AppName,
    ) where

import Data.Aeson         (object, (.=))
import Futurice.Prelude
import Prelude ()
import System.Directory   (copyFile, getCurrentDirectory)
import System.Environment (getEnvironment)
import System.Exit        (exitFailure)
import System.FilePath    ((</>))
import System.IO          (hClose, hFlush)
import System.IO.Temp     (withTempFile)
import System.Posix.Types (UserID)
import System.Posix.User  (getRealUserID)
import System.Process     (callProcess, readProcess)

import qualified Data.Map            as Map
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified System.Console.ANSI as ANSI
import qualified Text.Microstache    as M

import Futurice.App.MegaRepoTool.Config

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

buildCmd :: Text -> Cmd
buildCmd buildImage = cmdUnwords
    [ "docker run"
    , "--rm"
    , "-ti"
    , "-e DOCKER=YES" -- tell script we are in docker
    , "-e ORIGINAL_UID=" <> cmdUid
    , "-v " <> cmdPwd <> ":/app/src" -- cannot _yet- have ":ro".
    , "-v haskell-mega-repo-cabal:/root/.cabal"
    , "-v haskell-mega-repo-dist:/app/src/dist-newstyle-prod"
    , cmdText buildImage
    , "/app/src/scripts/build-in-linux-cabal.sh"
    ]

-------------------------------------------------------------------------------
-- Expansion
-------------------------------------------------------------------------------

data Frag
    = Frag Text
    | FragEnv String
    | FragPwd
    | FragUid
  deriving Show

cmdText :: Text -> Cmd
cmdText t = Cmd [Frag t]

-- | currently unused
-- cmdEnv :: String -> Cmd
-- cmdEnv e = Cmd [FragEnv e]

cmdPwd :: Cmd
cmdPwd = Cmd [FragPwd]

cmdUid :: Cmd
cmdUid = Cmd [FragUid]

newtype Cmd = Cmd [Frag]
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

cmdUnwords :: [Cmd] -> Cmd
cmdUnwords []     = mempty
cmdUnwords [c]    = c
cmdUnwords (c:cs) = c <> " " <> cmdUnwords cs

instance IsString Cmd where
    fromString s = Cmd [Frag (fromString s)]

cmdToText :: Cmd -> Text
cmdToText (Cmd frags) = foldMap toText frags
  where
    toText (Frag t)    = t
    toText (FragEnv s) = "$" <> fromString s
    toText FragPwd     = "$(pwd)"
    toText FragUid     = "$UID"

cmdExpand :: Cmd -> IO Text
cmdExpand (Cmd frags) = do
    pwd  <- getCurrentDirectory
    envs <- Map.fromList <$> getEnvironment
    uid  <- getRealUserID
    return $ foldMap (f pwd envs uid) frags
  where
    f :: String -> Map String String -> UserID -> Frag -> Text
    f _pwd _envs _uid (Frag t)    = t
    f _pwd  envs _uid (FragEnv s) = maybe "" (view packed) $ envs ^? ix s
    f  pwd _envs _uid FragPwd     = pwd ^. packed
    f _pwd _envs  uid FragUid     = textShow uid

-------------------------------------------------------------------------------
-- Build command
-------------------------------------------------------------------------------

cmdBuildCommand :: IO ()
cmdBuildCommand = do
    cfg <- readConfig
    cmd <- cmdExpand $ buildCmd $ _mrtDockerBaseImage cfg
    T.putStrLn cmd

-------------------------------------------------------------------------------
-- Build image
-------------------------------------------------------------------------------

cmdBuildDocker :: [AppName] -> IO ()
cmdBuildDocker appnames = do
    -- Read config
    cfg <- readConfig

    -- `some` verifies images aren't empty
    when (null appnames) $ do
        putStrLn "Image names are required"
        exitFailure

    -- What apps to build?
    apps <- fmap Map.fromList $ for appnames $ \appname -> do
        case cfg ^? mrtApps . ix appname of
            Nothing -> do
                putStrLn $ "Unknown app: " <> appname ^. unpacked
                exitFailure
            Just app -> pure (appname, app)

    -- Get the hash of current commit
    githash' <- readProcess "git" ["log", "--pretty=format:%h", "-n", "1", "--abbrev=8"] ""
    _        <- readProcess "git" ["rev-parse", "--verify", githash'] ""
    let githash = githash' ^. packed
    T.putStrLn $ "Git hash aka tag for images: " <> githash

    -- Check that binaries are build with current hash
    githashBuild <- (T.strip <$> T.readFile "build/git-hash.txt") `catch` onIOError "<none>"

    when (githashBuild /= githash) $ do
        T.putStrLn $ "Git hash in build directory don't match: " <> githashBuild  <> " != " <> githash
        T.putStrLn $ "Make sure you have data volumes:"
        ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green]
        T.putStrLn $ "    docker volume create --name haskell-mega-repo-cabal"
        T.putStrLn $ "    docker volume create --name haskell-mega-repo-dist"
        ANSI.setSGR []
        T.putStrLn $ "Run following command to build image:"
        ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green]
        T.putStrLn "    $(mega-repo-tool build-cmd)"
        ANSI.setSGR []
        T.putStrLn "  or"
        ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Blue]
        T.putStrLn $ "    " <> cmdToText (buildCmd $ _mrtDockerBaseImage cfg)
        ANSI.setSGR []
        exitFailure

    -- Build docker images
    images <- ifor apps $ \appname (ImageDefinition image exe restart) -> do
        -- Write Dockerfile
        dockerfile <- makeDockerfile exe cfg
        T.putStrLn dockerfile
        let directory = "build" </> exe ^. unpacked
        withTempFile directory "Dockerfile." $ \fp handle -> do
            -- Because of
            -- https://bugs.launchpad.net/ubuntu/+source/graphviz/+bug/1409280
            -- we have own sfdp
            --
            -- To compile own use sfdp
            -- /configure --prefix=/opt/graphviz --with-gtk=no --with-glade=no --with-glut=no --with-gts=yes --with-smyrna=no --with-pangocairo=yes --enable-static --disable-shared
            copyFile "vendor/sfdp" (directory </> "sfdp")

            -- write dockerfile
            T.hPutStrLn handle dockerfile
            hFlush handle
            hClose handle

            -- Build an image
            let fullimage = "futurice/" <> image <> ":" <> githash
            callProcess "docker" ["build", "-t", T.unpack fullimage, "-f", fp, directory]

            -- accumulate image names
            pure (appname, fullimage, "futurice/" <> image, restart)

    T.putStrLn "Upload images by:"
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green]
    for_ images $ \(_, image, _, _) ->
        T.putStrLn $ "  docker push " <> image
    ANSI.setSGR []

    T.putStrLn "Deploy images by:"

    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Dull ANSI.Green]
    for_ images $ \(appname, _, image, restart) -> do
        T.putStrLn $ "  appswarm app:deploy"
            <> " --name " <> appname
            <> " --image " <> image
            <> " --tag " <> githash

        for_ restart $ \appname' -> T.putStrLn $ "  appswarm app:restart"
            <> " --name " <> appname'

    ANSI.setSGR []

makeDockerfile :: Text -> MRTConfig -> IO Text
makeDockerfile exe cfg = do
    traverse_ (putStrLn . M.displayMustacheWarning) warns
    return (rendered ^. strict)
  where
    tmpl = _mrtDockerfileTmpl cfg
    debs =  T.intercalate " " (_mrtDebs cfg)
    (warns, rendered) = M.renderMustacheW tmpl $ object
        [ "debs" .= debs
        , "exe" .= exe
        ]

onIOError :: Monad m => a -> IOError -> m a
onIOError v _ = return v
