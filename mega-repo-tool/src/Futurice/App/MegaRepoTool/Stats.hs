{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
module Futurice.App.MegaRepoTool.Stats (
    stats,
    ) where

import Data.List                (isSuffixOf)
import Data.Machine
import Data.Machine.Runner      (foldT)
import Data.Semigroup           (Max (..))
import Data.Semigroup.Generic   (gmappend, gmempty)
import Data.TDigest             (TDigest, singleton)
import Data.TDigest.Postprocess (HistBin, cdf, histogram, quantile)
import Futurice.Monoid          (Average (..))
import Futurice.Prelude
import Prelude ()
import Text.Printf              (printf)

import qualified Data.ByteString.Char8 as BS8

directoryWalk' :: ([Char] -> Bool) -> MachineT IO (Is FilePath) a
directoryWalk' = undefined -- TODO

files :: ProcessT IO a [Char]
files = undefined --TODO
-------------------------------------------------------------------------------
-- Stats monoid
-------------------------------------------------------------------------------

-- | TODO: collect other stats as well
data Stats = Stats
    { _statsFiles    :: !(Sum Int)
    , _statsLines    :: !(Sum Int)
    , _statsNELines  :: !(Sum Int)
    , _statsLinesAvg :: !(Average Double)
    , _statsLinesMax :: !(Max Int)
    , _statsLinesTD  :: !(TDigest 100)
    }
    deriving (Show, Generic)

instance Semigroup Stats where
    (<>) = gmappend

instance Monoid Stats where
    mempty = gmempty
    mappend = (<>)

fileStats :: ByteString -> Stats
fileStats t = Stats
    { _statsFiles    = Sum $ 1
    , _statsLines    = Sum $ lineCount
    , _statsNELines  = Sum $ length . filter (not . BS8.null) $ ls
    , _statsLinesAvg = Average 1 $ fromIntegral lineCount
    , _statsLinesMax = Max $ lineCount
    , _statsLinesTD  = singleton $ fromIntegral lineCount
    }
  where
    ls        = BS8.lines t
    lineCount = length ls

-------------------------------------------------------------------------------
-- Machine
-------------------------------------------------------------------------------

statsMachine :: ProcessT IO FilePath Stats
statsMachine
    =  directoryWalk' dirPredicate
    ~> files
    ~> filtered filePredicate
    -- ~> autoM (\x -> print x >> return x)
    ~> autoM BS8.readFile
    ~> mapping fileStats
  where
    dirPredicate d = not . any ($ d) $
        [ isSuffixOf ".git"
        , isSuffixOf ".stack-work"
        , isSuffixOf ".stack-work-dev"
        -- build-in-docker.sh artifacts
        , isSuffixOf ".stack-root"
        , isSuffixOf ".stack-work-docker"
        , isSuffixOf "dist-newstyle"
        , isSuffixOf "dist-newstyle-prod"
        -- js
        , isSuffixOf "node_modules"
        -- magic directories
        , isSuffixOf "vendor"
        , isSuffixOf "venv"
        , isSuffixOf "deprecated" -- some code we just have around
        ]

    filePredicate f = (isSuffixOf ".hs" f || isSuffixOf ".js" f || isSuffixOf ".java" f)
        && not (isSuffixOf "Setup.hs" f)
        && not (isSuffixOf "Main.hs" f)

stats :: IO ()
stats = do
    Stats fs ls nels lavg lmax td <- foldT (statsMachine <~ source ["."])
    let Just hist = histogram td
    putStrLn $ "total files:     " ++ show (getSum fs)
    putStrLn $ "total lines:     " ++ show (getSum ls)
    putStrLn $ "non-empty lines: " ++ show (getSum nels)
    putStrLn $ "average lines:   " ++ printf "%2.2f" (getAverage lavg)
    putStrLn $ "line count 10%:  " ++ printf "%2.2f" (runIdentity $ quantile 0.1 hist)
    putStrLn $ "line count 25%:  " ++ printf "%2.2f" (runIdentity $ quantile 0.25 hist)
    putStrLn $ "line count 50%:  " ++ printf "%2.2f" (runIdentity $ quantile 0.5 hist)
    putStrLn $ "line count 75%:  " ++ printf "%2.2f" (runIdentity $ quantile 0.76 hist)
    putStrLn $ "line count 90%:  " ++ printf "%2.2f" (runIdentity $ quantile 0.9 hist)
    putStrLn $ "line count 95%:  " ++ printf "%2.2f" (runIdentity $ quantile 0.95 hist)
    putStrLn $ "line count 98%:  " ++ printf "%2.2f" (runIdentity $ quantile 0.98 hist)
    putStrLn $ "max lines:       " ++ show (getMax lmax)
    traverse_ putStrLn $ plotTDigest hist

plotTDigest :: NonEmpty HistBin -> [String]
plotTDigest hist = map render $ dropLast $ map post buckets
  where
    mi = 0
    ma = runIdentity (quantile 0.98 hist)

    bucketMa = maximum (map snd buckets)
    buckets = steps 0 mi
    size = (ma - mi) / height
    steps !acc x
        | x >= ma   = []
        | otherwise = (y, acc' - acc) : steps acc' y
      where
        y = x + size
        acc' = cdf y hist

    post (y, b) = (y, round (width * b / bucketMa))
    dropLast = reverse . dropWhile ((== 0) . snd) . reverse
    render (y, n) = printf "%8.2f" y ++ " " ++ replicate n '#'

    height = 25
    width = 50
