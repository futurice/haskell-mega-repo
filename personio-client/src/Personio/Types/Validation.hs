{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Personio.Types.Validation where

import Control.Monad.Writer        (WriterT, execWriterT, unless)
import Data.Aeson.Types            (typeMismatch)
import Data.Char                   (ord)
import Data.List                   (foldl')
import Data.Maybe                  (isJust, isNothing)
import Data.Swagger
       (defaultSchemaOptions, genericDeclareNamedSchemaUnrestricted)
import FUM.Types.Login             (loginRegexp)
import Futurice.Aeson
import Futurice.Company
import Futurice.CostCenter
import Futurice.Email              (emailRegexp)
import Futurice.Generics
import Futurice.Office
import Futurice.Prelude
import Futurice.Tribe
import Prelude ()
import Text.Regex.Applicative.Text (RE', anySym, match, psym, string)

import Personio.Internal.Attribute
import Personio.Types.ContractType
import Personio.Types.Employee
import Personio.Types.EmployeeId
import Personio.Types.EmploymentType
import Personio.Types.PersonalIdValidations
import Personio.Types.SalaryType
import Personio.Types.Status

import qualified Data.Map.Strict as Map
import qualified Data.Text       as T

import Personio.Types.Internal

data ValidationMessage
    = CareerPathLevelMissing
    | ContractTypeMissing
    | SalaryTypeMissing
    | CostCenterMissing
    | CostCenterMultiple [Text]
    | CostCenterTribeMissMatch !CostCenter
    | DEIDInvalid
    | DESVInvalid
    | EmailInvalid Text
    | EmergencyContactPhoneInvalid Text
    | EmploymentTypeMissing
    | EndOfExpatAssignmentMissing
    | ExpatBonusAndAllowanceCurrencyMissing
    | ExternalMonthlyVariableSalary
    | FirstNameMissing
    | FISSNInvalid
    | FixedTermEndDateMissing
    | PermanentEndDateSet
    | FlowdockInvalid
    | GBNINOInvalid
    | GenderMissing
    | GithubInvalid Text
    | HireDateMissing
    | HomeCityMissing
    | HomeCountryMissing
    | HomePhoneInvalid Text
    | HomeStreetAddressMissing
    | HomeTribeInvalid Text
    | HoursInvalid Scientific
    | HRNumberInvalid Int
    | IbanInvalid
    | IdentificationNumberMissing
    | LastNameMissing
    | LoginInvalid Text
    | NationalityMissing
    | OfficeMissing
    | PermanentExternal
    | PermanentContractEndDate
    | PositionMissing
    | PrivateEmailInvalid Text
    | PrivatePhoneInvalid Text
    | RoleMissing
    | SalaryCurrencyInvalid Text
    | SalaryInvalid Text
    | SEHolidaysInvalid Scientific
    | SEPensionInvalid Scientific
    | SEPersonalIdInvalid
    | StartOfExpatAssignmentMissing
    | SupervisorMissing
    | TribeMissing
    | WorkPermitEndsMissing
    | WorkPermitMissing
    | WorkPhoneMissing
    | SupervisorNotActive !Text
    | OfficeCountryDontMatch Office (Maybe Country)
    | EmployerCountryDontMatch (Maybe Country) Company
    | StartDateMissing
    | EndDateBeforeStart Day Day
  deriving stock (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (NFData)

instance ToJSON ValidationMessage
instance FromJSON ValidationMessage
instance ToSchema ValidationMessage where declareNamedSchema = genericDeclareNamedSchemaUnrestricted defaultSchemaOptions

-- | All fields except 'evMessages' are to help connect the data.
data EmployeeValidation = EmployeeValidation
    { _evEmployee   :: !Employee
    , _evMessages   :: ![ValidationMessage]
    }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

makeLenses ''EmployeeValidation
deriveGeneric ''EmployeeValidation

deriveVia [t| FromJSON EmployeeValidation `Via` Sopica EmployeeValidation |]
deriveVia [t| ToJSON EmployeeValidation `Via` Sopica EmployeeValidation |]

instance ToSchema EmployeeValidation where declareNamedSchema = sopDeclareNamedSchema

-- | Validate collection of employees.
postValidatePersonioEmployees :: [EmployeeValidation] -> [EmployeeValidation]
postValidatePersonioEmployees ev = map single ev
  where
    m :: Map EmployeeId Employee
    m = Map.fromList $ map (\v -> (v ^. evEmployee . employeeId, v ^. evEmployee)) ev

    single :: EmployeeValidation -> EmployeeValidation
    single v = case msupervisor of
        Just supervisor | supervisor ^. employeeStatus /= Active -> v
            & evMessages %~ (SupervisorNotActive (supervisor ^. employeeFullname) :)
        -- otherwise, everything seems to be ok.
        _ -> v
      where
        msupervisor :: Maybe Employee
        msupervisor = do
            supId <- v ^. evEmployee . employeeSupervisorId
            m ^? ix supId

validatePersonioEmployee :: Value -> Parser EmployeeValidation
validatePersonioEmployee = withObjectDump "Personio.Employee" $ \obj -> do
    type_ <- obj .: "type"
    if type_ /= ("Employee" :: Text)
    then fail $ "Not Employee: " ++ type_ ^. unpacked
    else do
        rawAttrs <- obj .: "attributes"
        e <- parseEmployeeObject rawAttrs
        EmployeeValidation e <$> validate e (mkAttributes rawAttrs)
  where
    validate :: Employee -> Attributes -> Parser [ValidationMessage]
    validate e obj = execWriterT $ sequenceA_
        [ attributeMissing "first_name" FirstNameMissing
        -- TODO?
        -- , attributeMissing "gender" GenderMissing
        , attributeMissing "hire_date" HireDateMissing
        , attributeMissing "last_name" LastNameMissing
        -- , attributeMissing "position" PositionMissing
        , attributeObjectMissing "department" TribeMissing
        , attributeObjectMissing "office" OfficeMissing
        , costCenterValidate
        , dynamicAttributeMissing "Contract type" ContractTypeMissing
        , when isInternal $ dynamicAttributeMissing "Salary type" SalaryTypeMissing
        , when isInternal $ dynamicAttributeMissing "Home city" HomeCityMissing
        , when isInternal $ dynamicAttributeMissing "Home country" HomeCountryMissing
        , when isInternal $ dynamicAttributeMissing "Home street address" HomeStreetAddressMissing
        -- TODO: should require for externals too
        , when isInternal $ dynamicAttributeMissing "Primary role" RoleMissing
        , dynamicAttributeMissing "Work phone" WorkPhoneMissing
        , emailValidate
        , expatBonusAndAllowanceCurrencyValidate
        , expatValidate
        , externalContractValidate
        , fixedEndDateValidate
        , permanentEndDateValidate
        , githubValidate
        , homePhoneValidate
        , homeTribeValidate
        , hoursValidate
        , hrNumberValidate
        , ibanValidate
        , identificationNumberMissing
        , internalValidations
        , loginValidate
        , monthlyVariableSalaryValidate
        -- TODO: , phoneValidate "Emergency contact phone" EmergencyContactPhoneInvalid
        , privateEmailValidate
        , supervisorValidate
        , withValidatorValidate "(DE) Tax ID number" DEIDInvalid isValidDeID
        , withValidatorValidate "(DE) Social security number (SV)" DESVInvalid isValidDeSV
        , withValidatorValidate "(FI) Social Security Number" FISSNInvalid isValidFinSSN
        , withValidatorValidate "(GB) National Insurance Number" GBNINOInvalid isValidGbNINO
        , withValidatorValidate "(SE) Personal number" SEPersonalIdInvalid isValidSwePIN
        , workPermitEndsMissing
        , officeCountryDontMatch
        , employerCountryDontMatch
        , startDateMissing
        , endDateBeforeStart
        ]
      where
        isExternal = e ^. employeeEmploymentType == Just External
        isInternal = e ^. employeeEmploymentType == Just Internal
        isExpat    = e ^. employeeExpat

        privEmailRegexp = some anySym *> string "@" *> some anySym *> string "." *> some anySym

        phoneRegexp = string "+" *> (T.pack <$> some (psym (`elem` allowedChars)))
          where
            allowedChars = ' ':'-':['0'..'9']

        salarySet :: Value -> Bool
        salarySet (Number s) = s > 0
        salarySet _          = False

        isSomeText :: Value -> Maybe Text
        isSomeText (String v) = match (T.pack <$> some anySym :: RE' Text) v
        isSomeText _          = Nothing

        checkAttributeName :: Text -> ValidationMessage -> WriterT [ValidationMessage] Parser ()
        checkAttributeName val msg = case isSomeText (String val) of
            Nothing -> tell [msg]
            Just _  -> pure ()

        -- | Given attribute should be fetchable with parseAttribute,
        -- and error message should be constant value
        attributeMissing :: Text -> ValidationMessage -> WriterT [ValidationMessage] Parser ()
        attributeMissing attrName errMsg = do
            attribute <- lift (parseAttribute obj attrName)
            case attribute of
                Null     -> tell [errMsg]
                Array _  -> tell [errMsg]
                String a -> checkAttributeName a errMsg
                a        -> lift (typeMismatch (show attrName) a)

        -- | Attribute should be fetchable with parseAttribute,
        -- error message should be value constant and fetched attribute's value
        -- should be an object
        attributeObjectMissing :: Text -> ValidationMessage -> WriterT [ValidationMessage] Parser ()
        attributeObjectMissing attrName errMsg = do
            attribute <- lift (parseAttribute obj attrName)
            case attribute of
                Array _ -> tell [errMsg] -- Should not be an array!
                Null    -> tell [errMsg] -- Should not be an array!
                _       -> pure ()

        -- | Attribute should be fetchable with parseDynamicAttribute and error
        -- message should be a constant value
        dynamicAttributeMissing :: Text -> ValidationMessage -> WriterT [ValidationMessage] Parser ()
        dynamicAttributeMissing attrName errMsg = do
            attribute <- lift $ optional $ parseDynamicAttribute obj attrName
            case attribute of
                Nothing         -> tell [errMsg]
                Just Null       -> tell [errMsg]
                Just (Array _)  -> tell [errMsg]
                Just (String a) -> checkAttributeName a errMsg
                Just a          -> lift (typeMismatch (show attrName) a)

{-
        phoneValidate :: Text -> (Text -> ValidationMessage) ->  WriterT [ValidationMessage] Parser ()
        phoneValidate aName errMsg = do
            phone <- lift (parseDynamicAttribute obj aName)
            case match phoneRegexp phone of
                Just _ -> pure ()
                Nothing -> tell [errMsg phone]
-}

        githubValidate :: WriterT [ValidationMessage] Parser ()
        githubValidate = do
            githubText <- lift (parseDynamicAttribute obj "Github")
            case match (githubRegexp <|> pure "") githubText of
                Nothing -> tell [GithubInvalid githubText]
                Just _ -> pure ()

        costCenterValidate :: WriterT [ValidationMessage] Parser ()
        costCenterValidate = do
          ccs <- lift (parseAttribute obj "cost_centers")
          case ccs of
              []              -> tell [CostCenterMissing]
              [CostCenter' cc _p]
                  | cc `notElem` tribeCostCenters (e ^. employeeTribe)
                              -> tell [CostCenterTribeMissMatch cc]
                  -- TODO: verify percentage
                  | otherwise -> pure ()
              xs              -> tell [CostCenterMultiple (map (textShow . getCostCenter') xs)]

        ibanValidate :: WriterT [ValidationMessage] Parser ()
        ibanValidate = when isInternal $ do
            iban <- lift (parseDynamicAttribute obj "IBAN")
            case iban of
                String unparsed -> if isValidIBAN unparsed
                    then pure ()
                    else tell [IbanInvalid]
                i -> lift (typeMismatch "IBAN" i)

        loginValidate :: WriterT [ValidationMessage] Parser ()
        loginValidate = do
            login <- lift (parseDynamicAttribute obj "Login name")
            case match loginRegexp login of
                Nothing -> tell [LoginInvalid login]
                Just _  -> pure ()

        fixedEndDateValidate :: WriterT [ValidationMessage] Parser ()
        fixedEndDateValidate = when (e ^. employeeContractType == Just FixedTerm) $ do
            checkEndDate FixedTermEndDateMissing
          where
            checkEndDate err = do
              eDate <- lift (parseAttribute obj "contract_end_date")
              case eDate of
                  Null     -> tell [err]
                  String d -> checkAttributeName d err
                  _        -> pure ()

        permanentEndDateValidate :: WriterT [ValidationMessage] Parser ()
        permanentEndDateValidate = unless (e ^. employeeContractType == Just FixedTerm) $ do
            checkEndDate PermanentEndDateSet
          where
            checkEndDate err = do
              eDate <- lift (parseAttribute obj "contract_end_date")
              case eDate of
                  Null     -> pure ()
                  String _ -> tell [err] -- error if set
                  _        -> pure ()

        externalContractValidate :: WriterT [ValidationMessage] Parser ()
        externalContractValidate = when isExternal $ do
            cType <- lift (optional $ parseDynamicAttribute obj "Contract type")
            case cType of
                Just PermanentAllIn -> tell [PermanentExternal]
                Just Permanent      -> tell [PermanentExternal]
                _                   -> pure ()

        homePhoneValidate :: WriterT [ValidationMessage] Parser ()
        homePhoneValidate = do
            hPhone <- lift (parseDynamicAttribute obj "Private phone")
            case match (void phoneRegexp <|> pure ()) hPhone of
                Nothing -> tell [HomePhoneInvalid hPhone]
                Just () -> pure ()

        emailValidate :: WriterT [ValidationMessage] Parser ()
        emailValidate = do
            email <- lift (parseAttribute obj "email")
            case match emailRegexp email of
                Nothing -> tell [EmailInvalid email]
                Just _  -> pure ()

        supervisorValidate :: WriterT [ValidationMessage] Parser ()
        supervisorValidate = do
            position <- lift (parseAttribute obj "position")
            case match noSuperRegexp position of
                Nothing -> attributeObjectMissing "supervisor" SupervisorMissing
                Just _  -> pure ()
          where
            noSuperRegexp = string "CEO"

        hoursValidate :: WriterT [ValidationMessage] Parser ()
        hoursValidate = when isInternal $ do
            hours <- lift (parseAttribute obj "weekly_working_hours")
            case toNum hours of
                Just n  -> if n > 0
                    then pure ()
                    else tell [HoursInvalid n]
                Nothing -> lift (typeMismatch "weekly_working_hours" hours)
          where
            toNum :: Value -> Maybe Scientific
            toNum (String v) = readMaybe $ T.unpack v
            toNum _          = Nothing

        workPermitEndsMissing :: WriterT [ValidationMessage] Parser ()
        workPermitEndsMissing = when isInternal $ do
            pType <- lift (parseDynamicAttribute obj "Work permit")
            if T.toLower pType == "temporary"
                then dynamicAttributeMissing "Work permit ends on" WorkPermitEndsMissing
                else pure ()

        internalValidations :: WriterT [ValidationMessage] Parser ()
        internalValidations = when isInternal $ do
            dynamicAttributeMissing "Nationality" NationalityMissing
            dynamicAttributeMissing "Work permit" WorkPermitMissing
            dynamicAttributeMissing "Career level" CareerPathLevelMissing
            salaryCurrencyValidate
            sweValidations
            salaryValidate
          where
            salaryCurrencyValidate = do
                currency <- lift (parseDynamicAttribute obj "Salary currency")
                case currency of
                    String cur -> if isJust (currencyRegExp cur)
                                      then pure ()
                                      else tell [SalaryCurrencyInvalid cur]
                    _          -> lift (typeMismatch "Salary currency" currency)
              where
                currencyRegExp = match (string "EUR" <|> string "GBP" <|> string "SEK" <|> string "NOK")

            sweValidations = do
                nat <- lift (parseDynamicAttribute obj "Nationality")
                case nat of
                    String n -> when (T.toLower n == "sweden") $ do
                                    validatePension
                                    validateHolidays
                    _        -> lift (typeMismatch "Nationality" nat)

            validatePension = do
                pension <- lift (parseDynamicAttribute obj "(SE) Occupational pension %")
                case pension of
                    Number p -> if p >= 0
                        then pure ()
                        else tell [SEPensionInvalid p]
                    _        -> lift (typeMismatch "(SE) Occupational pension %" pension)

            validateHolidays = do
                hDays <- lift (parseDynamicAttribute obj "(SE) Holidays")
                case hDays of
                    Number h -> if h > 0
                        then pure ()
                        else tell [SEHolidaysInvalid h]
                    _        -> lift (typeMismatch "(SE) Holidays" hDays)

        privateEmailValidate :: WriterT [ValidationMessage] Parser ()
        privateEmailValidate = do
            pMail <- lift (parseDynamicAttribute obj "Private email")
            case match (void privEmailRegexp <|> pure ()) pMail of
                Just _  -> pure ()
                Nothing -> tell [PrivateEmailInvalid pMail]

        expatValidate :: WriterT [ValidationMessage] Parser ()
        expatValidate = do
            expat <- lift (parseDynamicAttribute obj "Expat")
            if String expat == "Yes"
                then do
                    dynamicAttributeMissing "Start of assignment" StartOfExpatAssignmentMissing
                    dynamicAttributeMissing "End of assignment" EndOfExpatAssignmentMissing
                else pure ()

        homeTribeValidate :: WriterT [ValidationMessage] Parser()
        homeTribeValidate = do
            hTribe <- lift (parseDynamicAttribute obj "Home tribe")
            unless (hTribe == "") $ case tribeFromText hTribe of
                Just _  -> pure ()
                Nothing -> tell [HomeTribeInvalid hTribe]

        expatBonusAndAllowanceCurrencyValidate :: WriterT [ValidationMessage] Parser ()
        expatBonusAndAllowanceCurrencyValidate = do
            eBonus <- lift (parseDynamicAttribute obj "Expat monthly bonus 100%")
            eAllow <- lift (parseDynamicAttribute obj "(OLD) Expat housing allowance")
            case (isSomeText eBonus, isSomeText eAllow) of
                (Nothing, Nothing) -> pure ()
                _                  -> dynamicAttributeMissing
                                          "Expat bonus and allowance currency"
                                          ExpatBonusAndAllowanceCurrencyMissing

        salaryValidate :: WriterT [ValidationMessage] Parser ()
        salaryValidate = do
            monthlyS <- lift (parseDynamicAttribute obj "Monthly fixed salary 100%")
            hourlyS <- lift (parseDynamicAttribute obj "Hourly salary")

            for_ (e ^. employeeSalaryType) $ \st ->
                if st == Monthly
                then do
                    when   (salarySet hourlyS)  $ tell [ SalaryInvalid "Type: Monthly: hourly salary set" ]
                    unless (salarySet monthlyS) $ tell [ SalaryInvalid "Type: Monthly: monthly salary not set" ]
                else do
                    unless (salarySet hourlyS)  $ tell [ SalaryInvalid "Type: Hourly: hourly salary not set" ]
                    when   (salarySet monthlyS) $ tell [ SalaryInvalid "Type: Hourly: monthly salary set" ]

        monthlyVariableSalaryValidate :: WriterT [ValidationMessage] Parser ()
        monthlyVariableSalaryValidate = do
            eType <- lift (parseAttribute obj "employment_type")
            case employmentTypeFromText eType of
                Just External -> variableSalaryNotSet
                _             -> pure ()
          where
            variableSalaryNotSet = do
                variableS <- lift (parseDynamicAttribute obj "Monthly variable salary 100%")
                when (salarySet variableS) $
                    tell [ExternalMonthlyVariableSalary]

        hrNumberValidate :: WriterT [ValidationMessage] Parser ()
        hrNumberValidate = when (isInternal && e ^. employeeOffice `elem` finnishOffices) $ do
            hrNum <- lift (parseDynamicAttribute obj "(FI) HR number" :: Parser Int)
            if hrNum > 0
            then pure ()
            else tell [HRNumberInvalid hrNum]

        withValidatorValidate :: Text -> ValidationMessage -> (Text -> Bool) -> WriterT [ValidationMessage] Parser ()
        withValidatorValidate attrN valMsg validation = do
            pId <- lift (parseDynamicAttribute obj attrN)
            case pId of
                String i -> if not $ T.null i
                    then unless (validation i) $ tell [valMsg]
                    else pure ()
                _        -> lift (typeMismatch (T.unpack attrN) pId)

        identificationNumberMissing :: WriterT [ValidationMessage] Parser ()
        identificationNumberMissing = when isInternal $ do
            fiSSN <- lift (parseDynamicAttribute obj "(FI) Social Security Number")
            deSSN <- lift (parseDynamicAttribute obj "(DE) Social security number (SV)")
            gbNIN <- lift (parseDynamicAttribute obj "(GB) National Insurance Number")
            sePIN <- lift (parseDynamicAttribute obj "(SE) Personal number")
            noPIN <- lift (parseDynamicAttribute obj "(NO) Personal identification number" <|> pure "")
            if (length . catMaybes $ map isSomeText [fiSSN, deSSN, gbNIN, sePIN, noPIN] ) > 0
                then pure ()
                else tell [IdentificationNumberMissing]

        officeCountryDontMatch :: WriterT [ValidationMessage] Parser ()
        officeCountryDontMatch = unless (isNothing c || c == Just (officeCountry o)) $
            tell [OfficeCountryDontMatch o c]
          where
            c = e ^. employeeCountry
            o = e ^. employeeOffice

        employerCountryDontMatch :: WriterT [ValidationMessage] Parser ()
        employerCountryDontMatch = cond (isJust c && c /= Just (companyCountry em)) $
            tell [EmployerCountryDontMatch c em]
          where
            cond | isExternal || not isExpat = when
                 | otherwise                 = unless

            c  = e ^. employeeCountry
            em = e ^. employeeEmployer

        startDateMissing :: WriterT [ValidationMessage] Parser ()
        startDateMissing = when (isNothing $ e ^. employeeHireDate) $
            tell [StartDateMissing]

        endDateBeforeStart :: WriterT [ValidationMessage] Parser ()
        endDateBeforeStart = case (e ^. employeeHireDate, e ^. employeeEndDate) of
            (Just hire, Just end) | hire > end -> tell [EndDateBeforeStart hire end]
            _ -> pure ()

        finnishOffices :: [Office]
        finnishOffices = filter (\o -> officeCountry o == countryFinland) [minBound .. maxBound]

-- | Validate IBAN.
--
-- See <https://en.wikipedia.org/wiki/International_Bank_Account_Number#Validating_the_IBAN>
--
isValidIBAN :: Text -> Bool
isValidIBAN = isValidIBANString . view unpacked
  where
    -- in this case it's way simpler to operate on list of characters!
    isValidIBANString :: String -> Bool
    isValidIBANString = validate . rearrange . filter (/= ' ')

    -- Move the four initial characters to the end of the string
    --
    -- Note: this preserves the length!
    rearrange :: [a] -> [a]
    rearrange xs = drop 4 xs ++ take 4 xs

    -- Replace each letter in the string with two digits
    --
    -- We replace characters into 'Integer -> Integer' functions, for example
    --
    -- '2' -> (\x -> x * 10 + 2)
    -- 'a' -> (\x -> x * 100 + 10)
    --
    -- etc.
    --
    -- Note: we need to fold from the left!
    --
    toDigit :: Char -> Maybe (Integer -> Integer)
    toDigit c
        | '0' <= c && c <= '9' = Just $ \n -> n * 10 + fromIntegral (ord c - ord '0')
        | 'A' <= c && c <= 'Z' = Just $ \n -> n * 100 + fromIntegral (ord c - ord 'A' + 10)
        | otherwise            = Nothing

    -- takes spaceless and re-arranged string
    validate :: String -> Bool
    validate xs = isJust $ do
        ds <- traverse toDigit xs  -- replace characters
        guard (15 <= length ds && length ds <= 31) -- length check
        -- Interpret the string as a decimal integer
        -- and compute the remainder of that number on division by 97
        let checksum = foldl' (&) 0 ds
        guard (checksum `mod` 97 == 1)
        pure ()
