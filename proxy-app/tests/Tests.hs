{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Prelude ()
import Futurice.Prelude
import Data.Binary.Tagged
import GHC.TypeLits          (natVal)
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified Futurice.GitHub      as GH
import qualified PlanMill.Types       as PM
import qualified PlanMill.Types.Query as PM
import qualified Data.ByteString.Base16.Lazy as Base16

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
            0 "695397a115a20d8eb68cdeafc6f44476dce0b4e3"
        , BTTest "planmill-haxl endpoint" (Proxy :: Proxy [Either Text PM.SomeResponse])
            0 "10703fb8de8a96a4d529a6d9dd096bef7bf4ec77"
        , BTTest "PlanMill.Projects" (Proxy :: Proxy PM.Projects)
            0 "23a7e05940eb50f2b32177a4d66aab0bae4dad80"
        , BTTest "PlanMill.Tasks" (Proxy :: Proxy PM.Tasks)
            0 "6f624274e2fdac20a4581177c66e194b460169a1"
        , BTTest "PlanMill.CapacityCalendars" (Proxy :: Proxy PM.CapacityCalendars)
            0 "1fde910b8a5fc395de41cf0cda34f17fe9d141cd"
        , BTTest "GitHub.SomeResponse" (Proxy :: Proxy GH.SomeResponse)
            0 "878a632b660c32fb9f126732f9809ba2ab2d0c7b"
        ]
