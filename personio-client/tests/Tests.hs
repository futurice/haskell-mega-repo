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
    , isValidIBANTests
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
        Active @=? e ^. employeeStatus
        Just 0 @=? e ^. employeeHRNumber

    , validations
    ]
  where
    contentsM = decodeStrict $(makeRelativeToProject "fixtures/employee.json" >>= embedFile)

-------------------------------------------------------------------------------
-- Validations
-------------------------------------------------------------------------------

validations :: TestTree
validations = testGroup "Validations"
    [ testValidation
        "GitHub"
        $(makeRelativeToProject "fixtures/employee-i-github.json" >>= embedFile)
        $ GithubInvalid "http://github.com/gitMastur"
    , testValidation
        "email"
        $(makeRelativeToProject "fixtures/employee-m-email.json" >>= embedFile)
        EmailMissing
    , testValidation
        "tribe"
        $(makeRelativeToProject "fixtures/employee-m-tribe.json" >>= embedFile)
        TribeMissing
    , testValidation
        "cost center"
        $(makeRelativeToProject "fixtures/employee.json" >>= embedFile)
        CostCenterMissing
    , testValidation
        "office"
        $(makeRelativeToProject "fixtures/employee-m-office.json" >>= embedFile)
        OfficeMissing
    , testValidation
        "phone"
        $(makeRelativeToProject "fixtures/employee-m-phone.json" >>= embedFile)
        PhoneMissing
    , testValidation
        "role"
        $(makeRelativeToProject "fixtures/employee-m-role.json" >>= embedFile)
        RoleMissing
    , testValidation
        "IBAN"
        $(makeRelativeToProject "fixtures/employee-i-iban.json" >>= embedFile)
        IbanInvalid
    , testValidation
        "login name"
        $(makeRelativeToProject "fixtures/employee-i-login.json" >>= embedFile)
        $ LoginInvalid "erAt"
    ]
  where
    testValidation name source warning = testCase name $ do
        contents <- decodeStrict source
        ev <- either fail pure $ parseEither validatePersonioEmployee contents
        assertBool (show ev) $ warning `elem` ev ^. evMessages

-------------------------------------------------------------------------------
-- IBAN
-------------------------------------------------------------------------------

isValidIBANTests :: TestTree
isValidIBANTests = testGroup "isValidIBAN"
    [ validIBAN "MT84 MALT 0110 0001 2345 MTLC AST0 01S"
    , invalidIBAN "FI21 Ä234 5600 0007 foo" -- invalid chars
    , invalidIBAN "FI21 1234 5600 00" -- too short
    , invalidIBAN "MT84 MALT 0110 0001 2345 MTLC AST0 01S 00" -- too long
    , invalidIBAN "FI21 4321 5600 0007 85" -- wrong checksum
    ]
  where
    validIBAN :: Text -> TestTree
    validIBAN iban = testCase (iban ^. unpacked ++ " is valid") $
        assertBool "invalid!" $ isValidIBAN iban

    invalidIBAN :: Text -> TestTree
    invalidIBAN iban = testCase (iban ^. unpacked ++ " is invalid") $
        assertBool "valid!" $ not $ isValidIBAN iban
