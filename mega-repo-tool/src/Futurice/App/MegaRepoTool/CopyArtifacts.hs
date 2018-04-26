module Futurice.App.MegaRepoTool.CopyArtifacts (
    cmdCopyArtifacts
    ) where

import Control.Lens (iforOf_, firstOf)
import Futurice.Prelude
import Prelude ()
import System.Directory (copyFile, createDirectoryIfMissing)
import Cabal.Plan
import System.Exit (exitFailure)
import System.FilePath.Posix ((</>))
import System.IO (stderr, hPutStrLn)
import System.Process (callProcess)

import qualified Data.Map                   as Map

import Futurice.App.MegaRepoTool.Config

cmdCopyArtifacts :: FilePath -> FilePath -> IO ()
cmdCopyArtifacts rootDir buildDir = do
    cfg <- readConfig
    (plan, _) <- findAndDecodePlanJson (Just buildDir)
    iforOf_ (mrtApps . ifolded) cfg $ \appName imgDef -> do
        fp <- maybe
            (hPutStrLn stderr ("Cannot find exe for " ++ show appName) >> exitFailure)
            return
            (findExe plan $ imgDef ^. idExecutable)

        let exe = imgDef ^. idExecutable . unpacked
        hPutStrLn stderr $ "Copying " ++ exe

        let dir = rootDir </> "build" </> exe
        createDirectoryIfMissing True dir
        copyFile fp (dir </> exe)

        -- also strip, to make it smaller
        callProcess "strip" [dir </> exe]

findExe :: PlanJson -> Text -> Maybe FilePath
findExe plan exe = firstOf (folded . _Just)
    [ ciBinFile ci
    | u <- Map.elems (pjUnits plan)
    , (CompNameExe exe', ci) <- Map.toList (uComps u)
    , exe == exe'
    ]
