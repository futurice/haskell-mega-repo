module Futurice.App.MegaRepoTool.Scripts (
    dotScript,
    packdepsScript,
    ) where

import Prelude ()
import Futurice.Prelude
import Control.Monad            ((>=>))
import Data.List                (isSuffixOf)
import Data.Machine
import System.Directory.Machine (directoryWalk', files)
import System.Process           (callProcess, readProcess)

-- |
-- @
-- packdeps "**/*.cabal"
-- @
packdepsScript :: IO ()
packdepsScript = do
    args   <- runT $ source ["."] ~> directoryWalk' dirPredicate ~> files ~> filtered filePredicate
    callProcess "packdeps" args
  where
    dirPredicate d = not . any ($ d) $
        [ isSuffixOf ".git"
        , isSuffixOf ".stack-work"
        , isSuffixOf ".stack-work-dev"
        -- build-in-docker.sh artifacts
        , isSuffixOf ".stack-root"
        , isSuffixOf ".stack-work-docker"
        ]

    filePredicate = isSuffixOf ".cabal"

dotScript :: IO ()
dotScript = () <$ pipe ""
  where
    pipe = readProcess "stack" ["dot"]
        >=> readProcess "tred" []
        >=> readProcess "sfdp" ["-Tpng", "-o", "deps.png" ]
