{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
import Data.Aeson.Compat
import Data.Aeson.Lens       (key, _String)
import Data.Aeson.Types      (parseEither)
import Futurice.Email        (mkEmail)
import Futurice.Office       (offTampere)
import Futurice.Prelude
import Futurice.Tribe        (mkTribe)
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
    , isValidFinSSNTests
    , isValidSwePINTests
    , isValidGbNINOTests
    , isValidDeSVTests
    , isValidDeId
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

correctEmployeeValue :: Value
correctEmployeeValue =
    $(makeRelativeToProject "fixtures/employee.json" >>= embedFromJSON (Proxy :: Proxy Value))

examples :: TestTree
examples = testGroup "HUnit"
    [ testCase "parsePersonioEmployee" $ do
        e <- either fail pure $
            parseEither parsePersonioEmployee correctEmployeeValue
        "Teemu" @=? e ^. employeeFirst
        "Teekkari" @=? e ^. employeeLast
        Just $(mkDay "2017-05-29") @=? e ^. employeeHireDate
        Nothing @=? e ^. employeeEndDate
        "Developer (Primary)" @=? e ^. employeeRole
        Just $(mkEmail "teemu.teekkari@futurice.com") @=? e ^. employeeEmail
        Just "+123 5678910" @=? e ^. employeeWorkPhone
        Just (EmployeeId 1337) @=? e ^. employeeSupervisorId
        $(mkTribe "Tammerforce") @=? e ^. employeeTribe
        offTampere @=? e ^. employeeOffice
        Just "gitMastur" @=? e ^. employeeGithub
        Nothing @=? e ^. employeeFlowdock
        Active @=? e ^. employeeStatus
        Just 0 @=? e ^. employeeHRNumber
        Just Internal @=? e ^. employeeEmploymentType
        Just "+123 5678910" @=? e ^. employeeHomePhone
    , validations
    ]

-------------------------------------------------------------------------------
-- Validations
-------------------------------------------------------------------------------

-- |
-- @
-- attributeValue :: Text -> Traversal' Value Value
-- @
attributeValue :: Applicative f => Text -> (Value -> f Value) -> Value -> f Value
attributeValue k = key "attributes" . key k . key "value"

validations :: TestTree
validations = testGroup "Validations"
    [ testValidation
        "GitHub"
        (GithubInvalid "http://github.com/gitMastur")
        $ correctEmployeeValue
            & attributeValue "dynamic_72913" . _String
                .~ "http://github.com/gitMastur"
    , testValidation
        "tribe"
        TribeMissing
        $ correctEmployeeValue
            & attributeValue "department" .~  Array mempty
    , testValidation
        "cost center"
        CostCenterMissing
        $ correctEmployeeValue -- TODO!
    , testValidation
        "office"
        OfficeMissing
        $ correctEmployeeValue
            & attributeValue "office" .~  Array mempty
    , testValidation
        "phone"
        WorkPhoneMissing
        $ correctEmployeeValue
            & attributeValue "dynamic_27163" . _String .~  ""
    , testValidation
        "role"
        RoleMissing
        $ correctEmployeeValue
            & attributeValue "dynamic_72910" . _String .~  ""
    , testValidation
        "IBAN"
        IbanInvalid
        $ correctEmployeeValue
            & attributeValue "dynamic_72976" . _String .~  "GB82 WEST 1234 568 7654 32"
    , testValidation
        "login name"
        (LoginInvalid "CAPS")
        $ correctEmployeeValue
            & attributeValue "dynamic_72982" . _String .~  "CAPS"
    , testValidation
        "fixed-term contract_end_date"
        FixedTermEndDateMissing
        $ correctEmployeeValue
            & attributeValue "dynamic_72935" . _String .~  "fixed term" -- contract type
            & attributeValue "contract_end_date" .~ Null
    , testValidation
        "permanent external"
        PermanentExternal
        $ correctEmployeeValue
            & attributeValue "dynamic_72935" . _String .~ "permanent" -- contract type
            & attributeValue "employment_type" . _String .~ "external"
{-
    , testValidation
        "home phone"
        (HomePhoneInvalid "123a4")
        $ correctEmployeeValue
            & attributeValue "dynamic_72936" . _String .~ "123a4"
-}
    , testValidation
        "flowdock"
        FlowdockInvalid
        $ correctEmployeeValue
            & attributeValue "dynamic_72914" . _String .~ "https://www.flowdock.com/12345" -- flowdock
    , testValidation
        "first name"
        FirstNameMissing
        $ correctEmployeeValue
            & attributeValue "first_name" . _String .~ ""
    , testValidation
        "last name"
        LastNameMissing
        $ correctEmployeeValue
            & attributeValue "last_name" . _String .~ ""
    {-
    , testValidation
        "gender"
        GenderMissing
        $ correctEmployeeValue
            & attributeValue "gender" . _String .~ ""
    -}
    , testValidation
        "email"
        (EmailInvalid "invalid.mail")
        $ correctEmployeeValue
            & attributeValue "email" . _String .~ "invalid.mail"
    , testValidation
        "hire-date"
        HireDateMissing
        $ correctEmployeeValue
            & attributeValue "hire_date" .~ Null
    , testValidation
        "supervisor"
        SupervisorMissing
        $ correctEmployeeValue
            & attributeValue "supervisor" .~ Array mempty
    , testValidation
        "weekly hours"
        (HoursInvalid 0)
        $ correctEmployeeValue
            & attributeValue "weekly_working_hours" . _String .~ "0"
    , testValidation
        "contract type"
        ContractTypeMissing
        $ correctEmployeeValue
            & attributeValue "dynamic_72935" . _String .~ "" -- Contract type
    , testValidation
        "nationality"
        NationalityMissing
        $ correctEmployeeValue
            & attributeValue "employment_type" . _String .~ "internal"
            & attributeValue "dynamic_66601" . _String .~ "" -- Nationality
    , testValidation
        "work permit"
        WorkPermitMissing
        $ correctEmployeeValue
            & attributeValue "employment_type" . _String .~ "internal"
            & attributeValue "dynamic_66604" . _String .~ "" -- Work permit
    , testValidation
        "work permit ends"
        WorkPermitEndsMissing
        $ correctEmployeeValue
            & attributeValue "dynamic_66604" . _String .~ "temporary" -- Work permit
            & attributeValue "dynamic_72937" .~ Null  -- Work permit ends
    , testValidation
        "career level"
        CareerPathLevelMissing
        $ correctEmployeeValue
            & attributeValue "employment_type" . _String .~ "internal"
            & attributeValue "dynamic_72940" .~ Null -- Career path level
    , testValidation
        "home street address"
        HomeStreetAddressMissing
        $ correctEmployeeValue
          & attributeValue "dynamic_72916" . _String .~ "" -- Home street address
    , testValidation
        "home city"
        HomeCityMissing
        $ correctEmployeeValue
          & attributeValue "dynamic_72918" . _String .~ "" -- Home city
    , testValidation
        "private email"
        (PrivateEmailInvalid "eskokarvinen@fi")
        $ correctEmployeeValue
            & attributeValue "dynamic_72921" . _String .~ "eskokarvinen@fi" -- private email
{-
    , testValidation
        "private phone"
        (PrivatePhoneInvalid "1234")
        $ correctEmployeeValue
            & attributeValue "dynamic_72938" . _String .~ "1234" -- private phone
    , testValidation
        "emergency contact phone"
        (EmergencyContactPhoneInvalid "1234")
        $ correctEmployeeValue
            & attributeValue "dynamic_72939" . _String .~ "1234" -- Emergency contact phone
-}
    , testValidation
        "start of assignment"
        StartOfExpatAssignmentMissing
        $ correctEmployeeValue
            & attributeValue "dynamic_72946" . _String .~ "Yes" -- Expat
            & attributeValue "dynamic_72950" . _String .~ "" -- Start of assignment
    , testValidation
        "end of assignment"
        EndOfExpatAssignmentMissing
        $ correctEmployeeValue
            & attributeValue "dynamic_72946" . _String .~ "Yes" -- Expat
            & attributeValue "dynamic_72949" . _String .~ "" -- End of assignment
    , testValidation
        "home tribe"
        (HomeTribeInvalid "SomeTribe")
        $ correctEmployeeValue
            & attributeValue "dynamic_72943" . _String .~ "SomeTribe" -- Home tribe
    , testValidation
        "expat bonus and allowance currency"
        ExpatBonusAndAllowanceCurrencyMissing
        $ correctEmployeeValue
            & attributeValue "dynamic_72971" . _String .~ "something?" -- Expat housing allowance
            & attributeValue "dynamic_72972" . _String .~ "" -- Expat bonus and allowance currency
    , testValidation
        "salary"
        (SalaryInvalid "Type: Monthly: monthly salary not set")
        $ correctEmployeeValue
            & attributeValue "employment_type" . _String .~ "internal"
            & attributeValue "dynamic_27160" .~ Number 0 -- Monthly fixed salary 100%
            & attributeValue "dynamic_27162" .~ Number 0 -- Hourly salary x 100
    , testValidation
        "monthly variable salary"
        ExternalMonthlyVariableSalary
        $ correctEmployeeValue
            & attributeValue "employment_type" . _String .~ "external"
            & attributeValue "dynamic_27161" .~ Number 42 -- Monthly variable salary 100%
    , testValidation
        "(SE) occupational pension %"
        (SEPensionInvalid (negate 1))
        $ correctEmployeeValue
            & attributeValue "employment_type" . _String .~ "internal"
            & attributeValue "dynamic_66601" . _String .~ "Sweden" -- Nationality
            & attributeValue "dynamic_27164" .~ Number (negate 1) -- (SE) Occupational pension %
    , testValidation
        "(SE) Holidays"
        (SEHolidaysInvalid 0)
        $ correctEmployeeValue
            & attributeValue "employment_type" . _String .~ "internal"
            & attributeValue "dynamic_66601" . _String .~ "Sweden" -- Nationality
            & attributeValue "dynamic_27166" .~ Number 0 -- (SE) Holidays
    , testValidation
        "HR number"
        (HRNumberInvalid 0)
        $ correctEmployeeValue
            & attributeValue "dynamic_66136" .~ Number 0 -- HR number
    , testValidation
       "(FI) social security number"
       FISSNInvalid
       $ correctEmployeeValue
           & attributeValue "dynamic_65812" . _String .~ "120464-126A" -- (FI) Social Security Number
    , testValidation
        "(SE) Personal number"
        SEPersonalIdInvalid
        $ correctEmployeeValue
            & attributeValue "dynamic_27168" . _String .~ "17001228-9U74" -- (SE) Personal number
    , testValidation
        "(GB) National Insurance Number"
        GBNINOInvalid
        $ correctEmployeeValue
            & attributeValue "dynamic_72941" . _String .~ "QQ 12 34 56 A" -- (GB) National Insurance Number
    , testValidation
        "(DE) Social security number (SV)"
        DESVInvalid
        $ correctEmployeeValue
            & attributeValue "dynamic_72931" . _String .~ "120308664019" -- (DE) Social security number (SV)
    , testValidation
        "(DE) ID number"
        DEIDInvalid
        $ correctEmployeeValue
            & attributeValue "dynamic_72928" . _String .~ "11222345671" -- (DE) ID number
    , testValidation
        "Salary currency"
        (SalaryCurrencyInvalid "ZWD")
        $ correctEmployeeValue
            & attributeValue "dynamic_27169" . _String .~ "ZWD" -- Salary currency
    , testValidation
        "Home country"
        HomeCountryMissing
        $ correctEmployeeValue
            & attributeValue "dynamic_72920" . _String .~ "" -- Home country
    , testValidation
        "Personal identification number"
        IdentificationNumberMissing
        $ correctEmployeeValue
            & attributeValue "dynamic_72931" . _String .~ "" -- (DE) Social security number (SV)
            & attributeValue "dynamic_72941" . _String .~ "" -- (GB) National Insurance Number
            & attributeValue "dynamic_27168" . _String .~ "" -- (SE) Personal number
            & attributeValue "dynamic_65812" . _String .~ "" -- (FI) Social Security Number
    ]
  where
    testValidation name warning val = testCase name $ do
        ev <- either fail pure $ parseEither validatePersonioEmployee val
        assertBool (show ev) $ warning `elem` ev ^. evMessages



validValue :: Show a => (a -> Bool) -> a ->  TestTree
validValue checker toCheck = testCase msg $
    assertBool "invalid!" $ checker toCheck
  where
    msg = show toCheck ++ " is valid"

invalidValue :: Show a => (a -> Bool) -> a -> TestTree
invalidValue checker toCheck = testCase msg $
    assertBool "valid!" $ not $ checker toCheck
  where
    msg = show toCheck ++ " is invalid"

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
    validIBAN = validValue isValidIBAN

    invalidIBAN :: Text -> TestTree
    invalidIBAN = invalidValue isValidIBAN

-------------------------------------------------------------------------------
-- Finnish SSN
-------------------------------------------------------------------------------

isValidFinSSNTests :: TestTree
isValidFinSSNTests = testGroup "isValidFinSSN"
    [ validSSN "120464-126J"
    , invalidSSN "1204-126J" -- too short
    , invalidSSN "12041964-126J" -- too long
    , invalidSSN "420464-126J" -- invalid day
    , invalidSSN "121364-126J" -- invalid month
    , invalidSSN "1204A9-126J" -- invalid year
    , invalidSSN "120464*126J" -- invalid century identifier
    , invalidSSN "120464-901J" -- invalid personal identifier
    , invalidSSN "120464-126A" -- invalid checksum-identifier
    ]
  where
    validSSN :: Text -> TestTree
    validSSN = validValue isValidFinSSN

    invalidSSN :: Text -> TestTree
    invalidSSN = invalidValue isValidFinSSN

isValidSwePINTests :: TestTree
isValidSwePINTests = testGroup "isValidSwePIN"
    [ validPIN "19811228-9874"
    , validPIN "19670919-9530"
    , invalidPIN "1981122-8987" -- too short
    , invalidPIN "199811228-9874" -- too long
    , invalidPIN "18911228-9874" -- invalid year
    , invalidPIN "19814228-9874" -- invalid month
    , invalidPIN "19811242-9874" -- invalid day
    , invalidPIN "19811228-9424" -- invalid personal identifier
    , invalidPIN "19811228-9878" -- invalid checksum-number
    ]
  where
    validPIN :: Text -> TestTree
    validPIN = validValue isValidSwePIN

    invalidPIN :: Text -> TestTree
    invalidPIN = invalidValue isValidSwePIN

isValidGbNINOTests :: TestTree
isValidGbNINOTests = testGroup "isValidGbNINO"
    [ validNINO "AE 12 34 56 A"
    , invalidNINO "AE 12 34 56" -- invalid length
    , invalidNINO "27 12 34 56 A" -- starting characters are not letters
    , invalidNINO "AE 12 E4 56 A" -- invalid date numbers
    , invalidNINO "AE 12 34 56 7" -- invalid suffix letter
    , invalidNINO "QD 12 34 56 A" -- D, F, I, Q, U, and V should not be used as either the first or second letter
    , invalidNINO "AO 12 34 56 A" -- The letter O should not be used as the second letter of a prefix
    , invalidNINO "BG 12 34 56 A" -- Prefixes BG, GB, KN, NK, NT, TN and ZZ should not be used
    ]
  where
    validNINO :: Text -> TestTree
    validNINO = validValue isValidGbNINO

    invalidNINO :: Text -> TestTree
    invalidNINO = invalidValue isValidGbNINO

isValidDeSVTests :: TestTree
isValidDeSVTests = testGroup "isValidDeSV"
    [ validSV "12030866A019"
    , invalidSV "12030866A08" -- too short
    , invalidSV "1203081966A019" -- too long
    , invalidSV "12420866A012" -- invalid day
    , invalidSV "12034266A013" -- invalid month
    , invalidSV "120308E3A011" -- invalid year
    , invalidSV "120308664017" -- invalid name initial
    , invalidSV "12030866A012" -- invalid checksum
    ]
  where
    validSV :: Text -> TestTree
    validSV = validValue isValidDeSV

    invalidSV :: Text -> TestTree
    invalidSV = invalidValue isValidDeSV

isValidDeId :: TestTree
isValidDeId = testGroup "isValidDeID"
    [ validID "10234567783"
    , invalidID "01234567896" -- 0 in beginning
    , invalidID "123456789014" -- too long
    , invalidID "1234567897" -- too short
    , invalidID "11112245672" -- digit appears more than thrice
    , invalidID "11222345671" -- more than one digits appear more than once
    , invalidID "12345678903" -- none appears twice or thrice
    , invalidID "10234567782" -- invalid checksum
    ]
  where
    validID :: Text -> TestTree
    validID = validValue isValidDeID

    invalidID :: Text -> TestTree
    invalidID = invalidValue isValidDeID
