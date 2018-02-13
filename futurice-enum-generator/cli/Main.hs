module Main (main) where

import Futurice.App.EnumGenerator
import System.Environment         (getArgs, getProgName)
import System.Exit                (exitFailure)
import System.IO                  (hPutStrLn, stderr)

import qualified Data.ByteString as BS

main :: IO ()
main = do
    args <- getArgs
    case dropWhile (/= "-h") args of
        -- GHC calls preprocessors like so:
        --
        -- > preprocessor [args] -h label Foo.lhs /tmp/somefile
        --
        -- [args] are custom arguments provided with -optL
        --
        -- The label is meant to be used in line pragmas, like so:
        --
        -- #line 1 "label"
        --
        ["-h", _, infile, outfile] -> do
            contents <- BS.readFile infile
            case generateEnum contents of
                Left err -> do
                    hPutStrLn stderr err
                    exitFailure
                Right bs -> BS.writeFile outfile bs
        _ -> do
            name <- getProgName
            hPutStrLn stderr $ "usage: " ++ name ++ " -h label infile outfile"
            exitFailure
