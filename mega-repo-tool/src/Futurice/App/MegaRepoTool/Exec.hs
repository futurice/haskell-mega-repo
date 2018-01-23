{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.MegaRepoTool.Exec (cmdExec) where

import Futurice.Prelude
import Prelude ()
import System.Environment (getEnvironment)
import System.Exit        (ExitCode (..))

import qualified Data.Map       as Map
import qualified System.Process as Process

import Futurice.App.MegaRepoTool.Config
import Futurice.App.MegaRepoTool.Keys   (Storage (..), readStorage)

cmdExec :: NonEmpty String -> IO ()
cmdExec (cmd :| args) = do
    storage <- readStorage
    cfg <- readConfig

    -- make the environment
    let secrets = storageOwn storage <> storageAll storage
    let value :: Either Text Text -> String
        value (Right v) = v ^. unpacked
        value (Left k)  = maybe "" (view unpacked) (secrets ^? ix k)
    procEnv <- Map.fromList <$> getEnvironment
    let env' = Map.mapKeys (view unpacked) $ fmap value (cfg ^. mrtEnvVars)
        env = Map.toList (env' <> procEnv)

    liftIO $ callProcessInEnv cmd args env

-------------------------------------------------------------------------------
-- Process extras
-------------------------------------------------------------------------------

callProcessInEnv :: FilePath -> [String] -> [(String, String)] -> IO ()
callProcessInEnv cmd args env = do
    (_, _, _, p) <- Process.createProcess $ (Process.proc cmd args)
        { Process.delegate_ctlc = True
        , Process.env           = Just env
        }
    exit_code <- Process.waitForProcess p
    case exit_code of
      ExitSuccess   -> return ()
      ExitFailure r -> error $ show r
