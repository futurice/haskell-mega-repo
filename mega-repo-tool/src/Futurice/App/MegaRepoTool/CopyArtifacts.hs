{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.MegaRepoTool.CopyArtifacts (
    CopyArtifactsOpts (..),
    cmdCopyArtifacts
    ) where

import Cabal.Plan
import Control.Lens          (firstOf, iforOf_)
import Futurice.Prelude
import Prelude ()
import AWS.Lambda.RuntimeAPI.Package
import System.Directory      (copyFile, createDirectoryIfMissing)
import System.Exit           (exitFailure)
import System.FilePath.Posix (takeFileName, (-<.>), (</>))
import System.IO             (hPutStrLn, stderr)
import System.IO.Temp        (withTempDirectory)
import System.Process        (callProcess)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map             as Map

import Futurice.App.MegaRepoTool.Config

data CopyArtifactsOpts = CAO
    { caoRootDir  :: !FilePath
    , caoBuildDir :: !FilePath
    , caoLambda   :: !Bool
    }
  deriving (Eq, Ord)

cmdCopyArtifacts :: CopyArtifactsOpts -> IO ()
cmdCopyArtifacts (CAO rootDir buildDir buildLambdas) = withTempDirectory "/tmp" "copy-artifacts" $ \tmpDir -> do
    cfg <- readConfig
    plan <- findAndDecodePlanJson (InBuildDir buildDir)

    -- lambdas
    when buildLambdas $ iforOf_ (mrtLambdas . ifolded) cfg $ \lambdaName lambdaDef -> do
        fp <- maybe
            (hPutStrLn stderr ("Cannot find exe for " ++ show lambdaName) >> exitFailure)
            return
            (findExe plan $ lambdaDef ^. ldExecutable)

        let conf = defaultConf
                & confReadFile       .~ readSoWithStrip tmpDir
                & confAdditionalLibs .~
                    -- PQ, and its deps (OpenSSL)
                    [ "libpq"
                    , "libasn1"
                    , "libcom_err"
                    , "libcrypt"
                    , "libcrypto"
                    , "libgnutls"
                    , "libgssapi"
                    , "libgssapi_krb5"
                    , "libhcrypto"
                    , "libheimbase"
                    , "libheimntlm"
                    , "libhogweed"
                    , "libhx509"
                    , "libidn"
                    , "libidn2"
                    , "libk5crypto"
                    , "libkeyutils"
                    , "libkrb5"
                    , "libkrb5support"
                    , "liblber-2"
                    , "libldap_r-2"
                    , "libnettle"
                    , "libp11-kit"
                    , "libresolv"
                    , "libroken"
                    , "libsasl2"
                    , "libsqlite3"
                    , "libssl"
                    , "libtasn1"
                    , "libunistring"
                    , "libwind"
                    -- C++
                    , "libstdc++"
                    -- GCC runtime
                    , "liblzma"
                    -- FFTW
                    , "libfftw3"
                    , "libfftw3f"
                    ]

        let lambdaName' = lambdaName ^. unpacked
        hPutStrLn stderr $ "Packaging " ++ lambdaName'

        bsl <- packageAwsLambda conf fp

        let dir = rootDir </> "build" </> "Lambdas"
        createDirectoryIfMissing True dir
        BSL.writeFile (dir </> lambdaName' -<.> "zip") bsl

    -- executables
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

readSoWithStrip :: FilePath -> FilePath -> IO BSL.ByteString
readSoWithStrip tmpDir soPath = do
    let name = tmpDir </> takeFileName soPath
    callProcess "strip" ["-o", name, soPath]
    BSL.readFile name

findExe :: PlanJson -> Text -> Maybe FilePath
findExe plan exe = firstOf (folded . _Just)
    [ ciBinFile ci
    | u <- Map.elems (pjUnits plan)
    , (CompNameExe exe', ci) <- Map.toList (uComps u)
    , exe == exe'
    ]
