{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
import Data.Aeson.Compat
import Data.Aeson.Types      (parseEither)
import Data.FileEmbed
import Futurice.Prelude
import Prelude ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Personio

main :: IO ()
main = defaultMain $ testGroup "tests"
    [ properties
    , examples
    ]

-------------------------------------------------------------------------------
-- Properties
-------------------------------------------------------------------------------

properties :: TestTree
properties = testGroup "QuickCheck"
    [ testProperty "Employee roundtrip" employeeAesonRoundtrip
    ]

employeeAesonRoundtrip :: Employee -> Property
employeeAesonRoundtrip e = lhs === rhs
  where
    lhs = eitherDecode (encode e)
    rhs = Right e

-------------------------------------------------------------------------------
-- Examples
-------------------------------------------------------------------------------

examples :: TestTree
examples = testGroup "HUnit"
    [ testCase "parsePersonioEmployee" $ do
        contents <- contentsM
        e <- either fail pure $ parseEither parsePersonioEmployee contents
        "Teemu" @=? e ^. employeeFirst
        "Teekkari" @=? e ^. employeeLast
        Just $(mkDay "2017-05-29") @=? e ^. employeeHireDate
        Nothing @=? e ^. employeeEndDate
        "Developer (Primary)" @=? e ^. employeeRole
        "teemu.teekkari@example.com" @=? e ^. employeeEmail
        "+123 5678910" @=? e ^. employeePhone
        Just (EmployeeId 1337) @=? e ^. employeeSupervisorId
        Just "A Tribe" @=? e ^. employeeTribe
        Just "Helsinki" @=? e ^. employeeOffice
        Just "gitMastur" @=? e ^. employeeGithub

    , testCase "validatePersonioEmployee validates GitHub" $ do
        contents <-  decodeStrict $(makeRelativeToProject "fixtures/employee-i-github.json" >>= embedFile)
        ev <- either fail pure $ parseEither validatePersonioEmployee contents
        assertBool (show ev) $
            GithubInvalid "http://github.com/gitMastur" `elem` ev ^. evMessages

    , testCase "validatePersonioEmployee validates email" $ do
        contents <-  decodeStrict $(makeRelativeToProject "fixtures/employee-m-email.json" >>= embedFile)
        ev <- either fail pure $ parseEither validatePersonioEmployee contents
        assertBool (show ev) $
            EmailMissing `elem` ev ^. evMessages

    , testCase "validatePersonioEmployee validates tribe" $ do
        contents <- decodeStrict $(makeRelativeToProject "fixtures/employee-m-tribe.json" >>= embedFile)
        ev <- either fail pure $ parseEither validatePersonioEmployee contents
        assertBool (show ev) $
            TribeMissing `elem` ev ^. evMessages

    , testCase "validatePeronioEmployee validates missing cost center" $ do
        contents <- contentsM
        ev <- either fail pure $ parseEither validatePersonioEmployee contents
        assertBool (show ev) $
            CostCenterMissing `elem` ev ^. evMessages

    , testCase "validatePersonioEmployee validates missing office" $ do
        contents <- decodeStrict $(makeRelativeToProject "fixtures/employee-m-office.json" >>= embedFile)
        ev <- either fail pure $ parseEither validatePersonioEmployee contents
        assertBool (show ev) $
            OfficeMissing `elem` ev ^. evMessages

    , testCase "validatePersonioEmployee validates missing Work phone" $ do
        contents <- decodeStrict $(makeRelativeToProject "fixtures/employee-m-phone.json" >>= embedFile)
        ev <- either fail pure $ parseEither validatePersonioEmployee contents
        assertBool (show ev) $
            PhoneMissing `elem` ev ^. evMessages

    , testCase "validatePersonioEmployee validates missing role" $ do
        contents <- decodeStrict $(makeRelativeToProject "fixtures/employee-m-role.json" >>= embedFile)
        ev <- either fail pure $ parseEither validatePersonioEmployee contents
        assertBool (show ev) $
            RoleMissing `elem` ev ^. evMessages

    , testCase "validatePersonioEmployee validates invalid IBAN" $ do
        contents <- decodeStrict $(makeRelativeToProject "fixtures/employee-i-iban.json" >>= embedFile)
        ev <- either fail pure $ parseEither validatePersonioEmployee contents
        assertBool (show ev) $
            IbanInvalid `elem` ev ^. evMessages

    , testCase "isValidIBAN validates correct IBAN" $ do
        correct <- pure "MT84 MALT 0110 0001 2345 MTLC AST0 01S"
        ev <- pure $ isValidIBAN correct
        assertBool (show correct) $ ev == True

    , testCase "isValidIBAN validates IBAN with invalid characters" $ do
        invalid <- pure "FI21 Ä234 5600 0007 foo"
        ev <- pure $ isValidIBAN invalid
        assertBool (show invalid) $ ev == False

    , testCase "isValidIBAN validates too short IBAN" $ do
        tooShort <- pure "FI21 1234 5600 00"
        ev <- pure $ isValidIBAN tooShort
        assertBool (show tooShort) $ ev == False

    , testCase "isvalidIBAN validates too long IBAN" $ do
        tooLong <- pure "MT84 MALT 0110 0001 2345 MTLC AST0 01S 00"
        ev <- pure $ isValidIBAN tooLong
        assertBool (show tooLong) $ ev == False

    , testCase "isValidIban validates IBAN with incorrect checksum" $ do
        invalid <- pure "FI21 4321 5600 0007 85"
        ev <- pure $ isValidIBAN invalid
        assertBool (show invalid) $ ev == False
    ]
  where
    contentsM = decodeStrict $(makeRelativeToProject "fixtures/employee.json" >>= embedFile)
