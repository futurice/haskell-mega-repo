{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Binary.Tagged
import Futurice.Prelude
import Prelude ()
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Futurice.GitHub      as GH
import qualified PlanMill.Types       as PM
import qualified PlanMill.Types.Query as PM

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "proxy-app"
    [ binaryTagTests
    ]

data BTTest where
    BTTest
        :: (Structured a)
        => String -> Proxy a -> String -> BTTest

binaryTagTests :: TestTree
binaryTagTests = testGroup "BinaryTagged tags" $ map mk tags
  where
    mk :: BTTest -> TestTree
    mk (BTTest name p h) = testProperty name $ once $
        h === showMD5 (structureHash p)

    tags :: [BTTest]
    tags =
        [ BTTest "PlanMill.SomeResponse" (Proxy :: Proxy PM.SomeResponse)
            "6cceee5d80e20682bc0882ac7b0256c7"
        , BTTest "planmill-haxl endpoint" (Proxy :: Proxy [Either Text PM.SomeResponse])
            "52d9c48c677c735f293c1055ef8c81a6"
        , BTTest "PlanMill.Projects" (Proxy :: Proxy PM.Projects)
            "086c56b9c08fd33d60517909c26cd80b"
        , BTTest "PlanMill.Tasks" (Proxy :: Proxy PM.Tasks)
            "a358a6fdfbb9ac5b33c5f58935d24da3"
        , BTTest "PlanMill.CapacityCalendars" (Proxy :: Proxy PM.CapacityCalendars)
            "8fc6cddac1228991014b13a4c3a50efe"
        , BTTest "GitHub.SomeResponse" (Proxy :: Proxy GH.SomeResponse)
            "adaf2653cff70e23f2792fff97c5ea6f"
        ]
