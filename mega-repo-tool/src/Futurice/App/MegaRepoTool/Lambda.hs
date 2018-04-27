{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.MegaRepoTool.Lambda (cmdLambda) where

import Futurice.Prelude
import PackageAwsLambda   (compilePython, findForeignLib, mkConf')
import Prelude ()
import System.Directory   (doesFileExist)
import System.Environment (getEnvironment)
import System.Exit        (exitFailure, exitWith)
import System.FilePath    (takeDirectory, (</>))
import System.IO.Temp     (withTempDirectory)
import System.Process.ByteString (readCreateProcessWithExitCode)

import qualified Data.Map       as Map
import qualified System.Process as Process
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Futurice.App.MegaRepoTool.Config
import Futurice.App.MegaRepoTool.Keys   (Storage (..), readStorage)

cmdLambda :: Text -> IO ()
cmdLambda lambdaName = do
    cfg <- readConfig

    lambdaDef <- maybe
        (putStrLn ("Unknown AWS Lambda " ++ show lambdaName) >> exitFailure)
        return
        (cfg ^. mrtLambdas . at lambdaName)

    _ <- Process.readProcess "cabal" ["new-build", lambdaDef ^. ldFlib . unpacked] ""

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
        BS.writeFile pyPath pyModule
        let env = Map.toList $ env' <> procEnv
                <> Map.singleton "PYTHONPATH" (takeDirectory pySo)
                <> Map.singleton "LD_LIBRARY_PATH" tmpDir
        let proc = (Process.proc "python2.7" ["Lambda.py"])
              { Process.cwd = Just tmpDir
              , Process.env = Just env
              }
        (ec, out, err) <- readCreateProcessWithExitCode proc mempty
        BS.putStr out
        BS.putStr err
        exitWith ec

pyModule :: ByteString
pyModule = BS8.unlines
    [ "from __future__ import print_function"
    , "import json"
    , "import Lambda_native"
    , "Lambda_native.hs_init(['tmpLambda', '+RTS', '-T'])"
    , "print(json.dumps(Lambda_native.handler('[]', None, print)))"
    ]
