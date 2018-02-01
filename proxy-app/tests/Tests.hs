{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Binary.Tagged
import Futurice.Prelude
import GHC.TypeLits          (natVal)
import Prelude ()
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Data.ByteString.Base16.Lazy as Base16
import qualified Futurice.GitHub             as GH
import qualified PlanMill.Types              as PM
import qualified PlanMill.Types.Query        as PM

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "proxy-app"
    [ binaryTagTests
    ]

data BTTest where
    BTTest
        :: (HasStructuralInfo a, HasSemanticVersion a)
        => String -> Proxy a -> Int -> LazyByteString -> BTTest

binaryTagTests :: TestTree
binaryTagTests = testGroup "BinaryTagged tags" $ map mk tags
  where
    mk :: BTTest -> TestTree
    mk (BTTest name p ver h) = testProperty name $ once $
        ver === fromIntegral (natVal (versionProxy p)) .&&.
        h === Base16.encode (structuralInfoSha1ByteStringDigest (structuralInfo p))

    versionProxy :: Proxy a -> Proxy (SemanticVersion a)
    versionProxy _ = Proxy

    tags :: [BTTest]
    tags =
        [ BTTest "PlanMill.SomeResponse" (Proxy :: Proxy PM.SomeResponse)
            0 "4c39408478af8aee8664ccd5ec8a44104e636336"
        , BTTest "planmill-haxl endpoint" (Proxy :: Proxy [Either Text PM.SomeResponse])
            0 "4756aadcbb45b19a0f4d0d0aa133733ce62071db"
        , BTTest "PlanMill.Projects" (Proxy :: Proxy PM.Projects)
            0 "23a7e05940eb50f2b32177a4d66aab0bae4dad80"
        , BTTest "PlanMill.Tasks" (Proxy :: Proxy PM.Tasks)
            0 "3e2a0ccee3c5e185019a407cab80db82ba304309"
        , BTTest "PlanMill.CapacityCalendars" (Proxy :: Proxy PM.CapacityCalendars)
            0 "1fde910b8a5fc395de41cf0cda34f17fe9d141cd"
        , BTTest "GitHub.SomeResponse" (Proxy :: Proxy GH.SomeResponse)
            0 "878a632b660c32fb9f126732f9809ba2ab2d0c7b"
        ]
