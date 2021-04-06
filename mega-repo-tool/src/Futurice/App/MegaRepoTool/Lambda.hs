module Futurice.App.MegaRepoTool.Lambda (cmdLambda, cmdDeployLambda) where

import Cabal.Plan
import Control.Lens                            (firstOf)
import Data.Aeson                              (encode)
import Data.Machine
       (MachineT (..), Step (..), await, repeatedly, runT_, (<~))
import Futurice.Prelude
import Prelude ()
import System.Directory                        (doesFileExist)
import System.Environment                      (getEnvironment)
import System.Exit
       (ExitCode (..), exitFailure, exitWith)
import System.IO                               (Handle, hFlush, hIsEOF, stdout)
import System.IO.Temp                          (withTempDirectory)
import System.Process                          (readProcess)
import Text.PrettyPrint.ANSI.Leijen.AnsiPretty (ansiPretty)
import Text.Printf                             (printf)

import qualified Control.Concurrent.Async as A
import qualified Data.ByteString          as BS
import qualified Data.Map                 as Map
import qualified Data.Text                as T
import qualified System.Process           as Process

import Futurice.App.MegaRepoTool.Config
import Futurice.App.MegaRepoTool.Keys   (Storage (..), readStorage)

cmdDeployLambda :: Text -> IO ()
cmdDeployLambda lambdaName = do
    let lambdaNameS = T.unpack lambdaName
    res <- readProcess "aws" ["lambda", "update-function-code", "--zip-file", "fileb://build/Lambdas/" <> lambdaNameS <> ".zip", "--function-name", lambdaNameS] ""
    print $ ansiPretty res

cmdLambda :: Text -> Maybe Value -> Maybe Int -> IO ()
cmdLambda lambdaName mpayload mtimelimit = do
    cfg <- readConfig
    plan <- findAndDecodePlanJson (InBuildDir "dist-newstyle")

    let timelimit = maybe 10 (max 1) mtimelimit
    let payload = maybe "null" (view unpacked . decodeUtf8Lenient . view strict . encode) mpayload

    lambdaDef <- maybe
        (putStrLn ("Unknown AWS Lambda " ++ show lambdaName) >> exitFailure)
        return
        (cfg ^. mrtLambdas . at lambdaName)

    (ecBuild, ()) <- readProcessMachines "cabal" ["new-build", lambdaDef ^. ldExecutable . unpacked] $ \mout merr ->
        A.runConcurrently $
            A.Concurrently (drain mout) *>
            A.Concurrently (drain merr)

    guard (ecBuild == ExitSuccess)

    exePath <- maybe
        (putStrLn ("Cannot find executable for " ++ show (lambdaDef ^. ldExecutable)) >> exitFailure)
        return
        (findExe plan $ lambdaDef ^. ldExecutable)

    exists <- doesFileExist exePath
    unless exists $ do
        putStrLn $ "File doesn't exist " ++ exePath
        exitFailure

    storage <- readStorage

    -- make the environment
    let secrets = storageOwn storage <> storageAll storage
    let value :: Either Text Text -> String
        value (Right v) = v ^. unpacked
        value (Left k)  = maybe "" (view unpacked) (secrets ^? ix k)
    procEnv <- Map.fromList <$> getEnvironment
    let env = Map.mapKeys (view unpacked) $ fmap value (cfg ^. mrtEnvVars)

    withTempDirectory "/tmp" "aws-lambda" $ \tmpDir -> do
        let proc = (Process.proc exePath [show timelimit, payload])
              { Process.cwd = Just tmpDir
              , Process.env = Just $ Map.toList $ env <> procEnv
              }
        (ts, (ec, ())) <- clocked $ readCreateProcessMachines proc $ \mout merr ->
            A.runConcurrently $ A.Concurrently (drain mout) *> A.Concurrently (drain merr)
        putStrLn $ printf "Elapsed %.02fs" (timeSpecToSecondsD ts)
        exitWith ec

drain :: MachineT IO k ByteString -> IO ()
drain m = runT_ $ sink <~ m where
    sink = repeatedly $ do
        bs <- await
        lift $ do
            BS.putStr bs
            hFlush stdout

findExe :: PlanJson -> Text -> Maybe FilePath
findExe plan exe = firstOf (folded . _Just)
    [ ciBinFile ci
    | u <- Map.elems (pjUnits plan)
    , (CompNameExe exe', ci) <- Map.toList (uComps u)
    , exe == exe'
    ]

-------------------------------------------------------------------------------
-- "machines-process"
-------------------------------------------------------------------------------

-- | Variant of 'readProcessWithExitCode'
readProcessMachines
    :: FilePath                                                              -- ^ Filename of the executable
    -> [String]                                                              -- ^ any arguments
    -> (MachineT IO k BS.ByteString -> MachineT IO k BS.ByteString -> IO c)  -- ^ continuation given stdout and stderr
    -> IO (ExitCode, c)
readProcessMachines cmd args = readCreateProcessMachines (Process.proc cmd args)

readCreateProcessMachines
    :: Process.CreateProcess
    -> (MachineT IO k BS.ByteString -> MachineT IO k BS.ByteString -> IO c)  -- ^ continuation given stdout and stderr
    -> IO (ExitCode, c)
readCreateProcessMachines cp0 kont =
    Process.withCreateProcess cp $ \Nothing (Just outh) (Just errh) ph -> do
        let mstdout = sourceHandleWith (flip BS.hGetSome 4096) outh
        let mstderr = sourceHandleWith (flip BS.hGetSome 4096) errh

        A.runConcurrently $ (,)
            <$> A.Concurrently (Process.waitForProcess ph)
            <*> A.Concurrently (kont mstdout mstderr)
  where
    cp = cp0
        { Process.std_in  = Process.NoStream
        , Process.std_out = Process.CreatePipe
        , Process.std_err = Process.CreatePipe
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
