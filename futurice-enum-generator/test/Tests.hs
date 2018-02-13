{-# LANGUAGE DeriveGeneric #-}
module Main (main) where

import Data.Algorithm.Diff        (Diff (..), getGroupedDiff)
import Futurice.Prelude
import Prelude ()
import System.Console.ANSI
       (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..),
       setSGRCode)
import System.FilePath            ((-<.>), (</>))
import Test.Tasty                 (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden.Advanced (goldenTest)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8

import Futurice.App.EnumGenerator

main :: IO ()
main = defaultMain $ testGroup "fixtures"
    [ fixture "TaskTag.lhs"
    ]

fixture :: FilePath -> TestTree
fixture fp = goldenTest fp (BS.readFile ref) act cmp upd
  where
    act = do
        contents <- BS.readFile $ "fixtures" </> fp
        return $ either BS8.pack id $ generateEnum contents

    ref = "fixtures" </> fp -<.> "output"
    upd = BS.writeFile ref

    cmp x y | x == y = return Nothing
    cmp x y = return $ Just $ rst ++ diff
      where
        rst = setSGRCode []
        red = setSGRCode [SetColor Foreground Vivid Red]
        grn = setSGRCode [SetColor Foreground Vivid Green]

        diff = unlines $ concatMap f (getGroupedDiff (BS8.lines x) (BS8.lines y))

        f (First xs)  = map ((red ++) . (++ rst) . cons3 '-' . BS8.unpack) xs ++ [rst]
        f (Second ys) = map ((grn ++) . (++ rst) . cons3 '+' . BS8.unpack) ys
        -- we print unchanged lines too. It shouldn't be a problem while we have
        -- reasonably small examples
        f (Both xs _) = map (cons3 ' ' . BS8.unpack) xs
        -- we add three characters, so the changed lines are easier to spot
        cons3 c cs = c : c : c : ' ' : cs

