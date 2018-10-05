{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Futurice.App.MegaRepoTool.Lambda (cmdLambda) where

import Data.Aeson         (encode, object, (.=))
import Data.Machine
       (MachineT (..), Step (..), await, repeatedly, runT_, (<~))
import Futurice.Clock     (clocked, timeSpecToSecondsD)
import Futurice.Prelude
import PackageAwsLambda   (compilePython, findForeignLib, mkConf')
import Prelude ()
import System.Directory   (doesFileExist)
import System.Environment (getEnvironment)
import System.Exit        (ExitCode (..), exitFailure, exitWith)
import System.FilePath    (takeDirectory, (</>))
import System.IO          (Handle, hFlush, hIsEOF, stdout)
import System.IO.Temp     (withTempDirectory)
import Text.Printf        (printf)

import qualified Control.Concurrent.Async as A
import qualified Data.ByteString          as BS
import qualified Data.Map                 as Map
import qualified System.Process           as Process
import qualified Text.Microstache         as MS
import qualified Text.RawString.QQ        as QQ

import Futurice.App.MegaRepoTool.Config
import Futurice.App.MegaRepoTool.Keys   (Storage (..), readStorage)

cmdLambda :: Text -> Maybe Value -> IO ()
cmdLambda lambdaName mpayload = do
    cfg <- readConfig

    lambdaDef <- maybe
        (putStrLn ("Unknown AWS Lambda " ++ show lambdaName) >> exitFailure)
        return
        (cfg ^. mrtLambdas . at lambdaName)

    (ecBuild, ()) <- readProcessMachines "cabal" ["new-build", lambdaDef ^. ldFlib . unpacked] $ \mout merr ->
        A.runConcurrently $
            A.Concurrently (drain mout) *>
            A.Concurrently (drain merr)

    guard (ecBuild == ExitSuccess)

    flibPath <- maybe
        (putStrLn ("Cannot find shared library" ++ show (lambdaDef ^. ldFlib)) >> exitFailure)
        return
        =<< findForeignLib Nothing (lambdaDef ^. ldFlib)

    exists <- doesFileExist flibPath
    unless exists $ do
        putStrLn $ "File doesn't exist " ++ flibPath
        exitFailure

    storage <- readStorage

    -- make the environment
    let secrets = storageOwn storage <> storageAll storage
    let value :: Either Text Text -> String
        value (Right v) = v ^. unpacked
        value (Left k)  = maybe "" (view unpacked) (secrets ^? ix k)
    procEnv <- Map.fromList <$> getEnvironment
    let env' = Map.mapKeys (view unpacked) $ fmap value (cfg ^. mrtEnvVars)

    let conf = mkConf' flibPath "Lambda" (lambdaDef ^. ldHandler) "handler"

    withTempDirectory "/tmp" "aws-lambda-py" $ \tmpDir -> do
        pySo <- compilePython tmpDir conf
        let pyPath = tmpDir </> "Lambda.py"
        BS.writeFile pyPath $ pyModule lambdaName mpayload
        let env = Map.toList $ env' <> procEnv
                <> Map.singleton "PYTHONPATH" (takeDirectory pySo)
                <> Map.singleton "LD_LIBRARY_PATH" tmpDir
        let proc = (Process.proc "python2.7" ["Lambda.py"])
              { Process.cwd = Just tmpDir
              , Process.env = Just env
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

pyModule :: Text -> Maybe Value -> ByteString
pyModule n v
    = encodeUtf8
    $ view strict
    $ MS.renderMustache lambdaPyTemplate
    $ object
        [ "name" .= n
        , "value" .= decodeUtf8Lenient (view strict (encode v))
        ]

lambdaPyTemplate :: MS.Template
Right lambdaPyTemplate = MS.compileMustacheText "lambda" [QQ.r|
from __future__ import print_function
import json
import pprint
import sys
import time
import Lambda_native

startClock = time.time()
timeLimit = 180  # seconds

class LambdaContext:
    function_name = '''{{{name}}}'''

    def get_remaining_time_in_millis(self):
        currClock = time.time();
        return int((timeLimit - (currClock - startClock)) * 1000)

def log(*args):
   print(*args)
   sys.stdout.flush()

Lambda_native.hs_init(['tmpLambda', '+RTS', '-T'])
pprint.pprint(json.loads(Lambda_native.handler('''{{{value}}}''', LambdaContext(), log)), indent=2)
|]

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
