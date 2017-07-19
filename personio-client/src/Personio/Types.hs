{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
module Personio.Types (
    module Personio.Types,
    module Personio.Types.EmployeeStatus,
    ) where

import Control.Monad.Writer
import Data.Aeson.Compat
import Data.Aeson.Internal         (JSONPathElement (Key), (<?>))
import Data.Aeson.Types
       (FromJSON1 (..), explicitParseField, parseJSON1, typeMismatch)
import Data.Char                   (ord)
import Data.List                   (foldl')
import Data.Maybe                  (isJust)
import Data.Time                   (zonedTimeToLocalTime)
import Futurice.Aeson
import Futurice.EnvConfig
import Futurice.Generics
import Futurice.IdMap              (HasKey (..))
import Futurice.Prelude
import Prelude ()
import Text.Regex.Applicative.Text (RE', anySym, match, psym, string)

import Personio.Types.EmployeeEmploymentType
       (EmploymentType (..), employmentTypeFromText)
import Personio.Types.EmployeeStatus         (Status (..))

import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

-- | Personio employee id.
newtype EmployeeId = EmployeeId Word
  deriving (Eq, Ord, Show)

deriveGeneric ''EmployeeId

instance Arbitrary EmployeeId where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance Hashable EmployeeId where
    hashWithSalt salt (EmployeeId i) = hashWithSalt salt i


instance FromJSON EmployeeId where
    parseJSON = fmap EmployeeId . parseJSON

instance ToJSON EmployeeId where
    toJSON (EmployeeId i) = toJSON i
    toEncoding (EmployeeId i) = toEncoding i

instance NFData EmployeeId where
    rnf (EmployeeId i) = rnf i

-- | We could use 'GeneralizedNewtypeDeriving', but we don't (yet?).
instance ToParamSchema EmployeeId where
    toParamSchema = newtypeToParamSchema

instance ToSchema EmployeeId where
    declareNamedSchema = newtypeDeclareNamedSchema

instance FromHttpApiData EmployeeId where
    parseUrlPiece = newtypeParseUrlPiece

instance ToHttpApiData EmployeeId where
    toUrlPiece = newtypeToUrlPiece

_EmployeeId :: Prism' Text EmployeeId
_EmployeeId = prism' toUrlPiece (either (const Nothing) Just . parseUrlPiece)

-- | Employee structure. Doesn't contain sensitive information.
data Employee = Employee
    { _employeeId             :: !EmployeeId
    , _employeeFirst          :: !Text
    , _employeeLast           :: !Text
    , _employeeHireDate       :: !(Maybe Day)
    , _employeeEndDate        :: !(Maybe Day)
    , _employeeRole           :: !Text
    , _employeeEmail          :: !Text
    , _employeePhone          :: !Text
    , _employeeSupervisorId   :: !(Maybe EmployeeId)
    , _employeeLogin          :: !(Maybe Text) -- TODO 4 or 5 lowercase letters `isLower` not good,
    , _employeeTribe          :: !(Maybe Text)
    , _employeeOffice         :: !(Maybe Text)
    , _employeeCostCenter     :: !(Maybe Text) -- exactly 1
    , _employeeGithub         :: !(Maybe Text)
    , _employeeStatus         :: !Status
    , _employeeHRNumber       :: !(Maybe Int)
    , _employeeEmploymentType :: !EmploymentType
    -- use this when debugging
    -- , employeeRest     :: !(HashMap Text Value)
    }
  deriving (Eq, Show, Generic)

makeLenses ''Employee
deriveGeneric ''Employee

instance NFData Employee

instance Arbitrary Employee where
    arbitrary = sopArbitrary
    shrink    = sopShrink

instance HasKey Employee where
    type Key Employee = EmployeeId
    key = employeeId

instance ToSchema Employee where
    declareNamedSchema = sopDeclareNamedSchema

instance FromJSON Employee where
    parseJSON = sopParseJSON

instance ToJSON Employee where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

parseAttribute :: FromJSON a => HashMap Text Attribute -> Text -> Parser a
parseAttribute obj attrName = case HM.lookup attrName obj of
    Nothing              -> fail $ "key " ++ show attrName ++ " not present"
    Just (Attribute _ v) -> parseJSON v <?> Key attrName

parseDynamicAttribute :: FromJSON a => HashMap Text Attribute -> Text -> Parser a
parseDynamicAttribute obj k = (dynamicAttributes obj) .: k
  where
    dynamicAttributes :: HashMap Text Attribute -> HashMap Text Value
    dynamicAttributes o = flip mapHM o $ \aKey (Attribute l v) ->
        if "dynamic_" `T.isPrefixOf` aKey
            then Just (l, v)
            else Nothing

    mapHM
            :: (Eq k2, Hashable k2)
            => (k1 -> v1 -> Maybe (k2, v2))
            -> HashMap k1 v1 -> HashMap k2 v2
    mapHM f = HM.fromList . mapMaybe (uncurry f) . HM.toList

parsePersonioEmployee :: Value -> Parser Employee
parsePersonioEmployee = withObjectDump "Personio.Employee" $ \obj -> do
    type_ <- obj .: "type"
    if type_ == ("Employee" :: Text)
        then obj .: "attributes" >>= parseObject
        else fail $ "Not Employee: " ++ type_ ^. unpacked
  where
    zonedDay = localDay . zonedTimeToLocalTime

    parseObject :: HashMap Text Attribute -> Parser Employee
    parseObject obj = Employee
        <$> parseAttribute obj "id"
        <*> parseAttribute obj "first_name"
        <*> parseAttribute obj "last_name"
        <*> fmap (fmap zonedDay) (parseAttribute obj "hire_date")
        <*> fmap (fmap zonedDay) (parseAttribute obj "contract_end_date")
        <*> parseDynamicAttribute obj "Primary role"
        <*> parseAttribute obj "email"
        <*> parseDynamicAttribute obj "Work phone"
        <*> fmap getSupervisorId (parseAttribute obj "supervisor")
        <*> fmap getMaybeLogin (parseDynamicAttribute obj "Login name")
        <*> fmap getName (parseAttribute obj "department")
        <*> fmap getName (parseAttribute obj "office")
        <*> fmap getName (parseAttribute obj "cost_centers")
        <*> fmap getGithubUsername (parseDynamicAttribute obj "Github")
        <*> parseAttribute obj "status"
        <*> parseDynamicAttribute obj "HR number"
        <*> parseAttribute obj "employment_type"
        -- <*> pure obj -- for employeeRest field

-- | Personio attribute, i.e. labelled value.
data Attribute = Attribute !Text !Value deriving Show

instance FromJSON Attribute where
    parseJSON = withObjectDump "Attribute" $ \obj -> Attribute
        <$> obj .: "label"
        <*> obj .: "value"

newtype SupervisorId = SupervisorId { getSupervisorId :: Maybe EmployeeId }

instance FromJSON SupervisorId where
    -- no supervisor: empty array
    parseJSON (Null) = pure (SupervisorId Nothing)
    parseJSON (Array xs) | null xs = pure (SupervisorId Nothing)
    parseJSON v = p v
      where
        p = withObjectDump "SupervisorId" $ \obj -> do
            type_ <- obj .: "type"
            if type_ == ("Employee" :: Text)
                then obj .: "attributes" >>= parseObject
                else fail $ "Attribute Supervisor is not Employee: " ++ type_ ^. unpacked

        parseObject :: HashMap Text Attribute -> Parser SupervisorId
        parseObject obj = SupervisorId <$> parseAttribute obj "id"

newtype NamedAttribute = NamedAttribute { getName :: Maybe Text }

instance FromJSON NamedAttribute where
    parseJSON v = case v of
        (Null)     -> pure (NamedAttribute Nothing)
        (Array xs) -> case toList xs of
            []    -> pure (NamedAttribute Nothing)
            (x:_) -> p x  -- take first attribute.
        _          -> p v
      where
        p = withObjectDump "NamedAttribute" $ \obj ->
            NamedAttribute . Just <$> ((obj .: "attributes") >>= (.: "name"))

newtype GithubUsername = GithubUsername { getGithubUsername :: Maybe Text }

instance FromJSON GithubUsername where
    parseJSON = withText "Github" (pure . GithubUsername . match githubRegexp)

githubRegexp :: RE' Text
githubRegexp = string "https://github.com/" *> (T.pack <$> some anySym)

newtype MaybeLogin = MaybeLogin { getMaybeLogin  :: Maybe Text }

instance FromJSON MaybeLogin where
    parseJSON = withText "Login" (pure . MaybeLogin . match loginRegexp)

loginRegexp :: RE' Text
loginRegexp = T.pack <$> range 4 5 (psym (`elem` ['a'..'z']))
  where
    range
        :: Alternative f
        => Int  -- ^ min
        -> Int  -- ^ max
        -> f a
        -> f [a]
    range mi ma f = go mi ma
      where
        go start end
            | start > end || end <= 0 = pure []
            | start > 0 = (:) <$> f <*> go (start - 1) (end - 1)
            | otherwise = inRange <$> optional f <*> go 0 (end - 1)

        inRange current next = maybe [] (:next) current

---------------------------------------------------------------------------
-- Envelope
-------------------------------------------------------------------------------

newtype Envelope a = Envelope { getEnvelope :: a }

instance FromJSON a => FromJSON (Envelope a) where
    parseJSON = parseJSON1

instance FromJSON1 Envelope where
    liftParseJSON p _ = withObjectDump "Envelope" $ \obj -> do
        b <- obj .: "success"
        case b of
            False -> do
                err <- obj .: "error"
                fail (errMessage err ^. unpacked)
            True -> Envelope <$> explicitParseField p obj "data"

-- | API error.
data Err = Err
    { errCode :: !Int
    , errMessage :: !Text
    }

instance FromJSON Err where
    parseJSON = withObjectDump "Error" $ \obj -> Err
        <$> obj .: "code"
        <*> obj .: "message"

-------------------------------------------------------------------------------
-- Credentials
-------------------------------------------------------------------------------

-- | Access Token
newtype AccessToken = AccessToken { _getAccessToken :: Text }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

makeLenses ''AccessToken
instance Hashable AccessToken
instance NFData AccessToken

instance IsString AccessToken where
    fromString = AccessToken . view packed

instance FromJSON AccessToken where
    parseJSON = withObjectDump "Personio.AccessToken" $ \obj ->
        AccessToken <$> obj .: "token"

-- | Client id
newtype ClientId = ClientId { _getClientId :: Text }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

makeLenses ''ClientId
instance Hashable ClientId
instance NFData ClientId

instance IsString ClientId where
    fromString = ClientId . view packed

instance FromJSON ClientId where
    parseJSON = withText "Personio.ClientId" $ pure . ClientId

-- | Client id
newtype ClientSecret = ClientSecret { _getClientSecret :: Text }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

makeLenses ''ClientSecret
instance Hashable ClientSecret
instance NFData ClientSecret

instance IsString ClientSecret where
    fromString = ClientSecret . view packed

instance FromJSON ClientSecret where
    parseJSON = withText "Personio.ClientSecret" $ pure . ClientSecret

-------------------------------------------------------------------------------
-- Base url
-------------------------------------------------------------------------------

-- | Base url of Persiono service
newtype BaseUrl = BaseUrl { _getBaseUrl :: Text }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

makeLenses ''BaseUrl
instance Hashable BaseUrl
instance NFData BaseUrl

instance IsString BaseUrl where
    fromString = BaseUrl . view packed

instance FromJSON BaseUrl where
    parseJSON = withText "FUM BaseUrl" $ pure . BaseUrl

productionBaseUrl :: BaseUrl
productionBaseUrl = BaseUrl "https://api.personio.de"

-------------------------------------------------------------------------------
-- Cfg
-------------------------------------------------------------------------------

data Cfg = Cfg
    { _cfgBaseUrl      :: !BaseUrl
    , _cfgClientId     :: !ClientId
    , _cfgClientSecret :: !ClientSecret
    }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

makeLenses ''Cfg
instance Hashable Cfg
instance NFData Cfg

class HasPersonioCfg a where
    personioCfg :: Lens' a Cfg

instance HasPersonioCfg Cfg where
    personioCfg = id

-------------------------------------------------------------------------------
-- env-config instances and helpers
-------------------------------------------------------------------------------

instance FromEnvVar ClientId where
    fromEnvVar = fmap ClientId . fromEnvVar

instance FromEnvVar ClientSecret where
    fromEnvVar = fmap ClientSecret . fromEnvVar

-- |
--
-- @
-- cfg <- withStderrLogger $ \logger -> runLogT "configure" logger $ getConfig' "" configurePersonioCfg
-- @
configurePersonioCfg :: ConfigParser Cfg
configurePersonioCfg = Cfg productionBaseUrl
    <$> envVar "PERSONIO_CLIENT_ID"
    <*> envVar "PERSONIO_CLIENT_SECRET"

instance Configure Cfg where
    configure = configurePersonioCfg

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------

data ValidationMessage
    = TribeMissing
    | EmailMissing
    | CostCenterMissing
    | CostCenterMultiple [Text]
    | GithubInvalid Text
    | OfficeMissing
    | RoleMissing
    | PhoneMissing
    | IbanInvalid
    | LoginInvalid Text
    | EmploymentTypeMissing
    | ExternalEndDateMissing
  deriving (Eq, Ord, Show, Typeable, Generic)


instance ToJSON ValidationMessage
instance FromJSON ValidationMessage
instance ToSchema ValidationMessage

-- | All fields except 'evMessages' are to help connect the data.
data EmployeeValidation = EmployeeValidation
    { _evEmployeeId :: !EmployeeId
    , _evFirst      :: !Text
    , _evLast       :: !Text
    , _evHireDate   :: !(Maybe Day)
    , _evEndDate    :: !(Maybe Day)
    , _evMessages   :: ![ValidationMessage]
    }
  deriving Show

makeLenses ''EmployeeValidation
deriveGeneric ''EmployeeValidation

instance ToSchema EmployeeValidation where
    declareNamedSchema = sopDeclareNamedSchema

instance FromJSON EmployeeValidation where
    parseJSON = sopParseJSON

instance ToJSON EmployeeValidation where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

validatePersonioEmployee :: Value -> Parser EmployeeValidation
validatePersonioEmployee = withObjectDump "Personio.Employee" $ \obj -> do
    type_ <- obj .: "type"
    if type_ == ("Employee" :: Text)
        then obj .: "attributes" >>= parseObject
        else fail $ "Not Employee: " ++ type_ ^. unpacked
  where
    parseObject :: HashMap Text Attribute -> Parser EmployeeValidation
    parseObject obj = EmployeeValidation
        <$> parseAttribute obj "id"
        <*> parseAttribute obj "first_name"
        <*> parseAttribute obj "last_name"
        <*> fmap (fmap zonedDay) (parseAttribute obj "hire_date")
        <*> fmap (fmap zonedDay) (parseAttribute obj "contract_end_date")
        <*> validate obj

    zonedDay =  localDay . zonedTimeToLocalTime

    validate :: HashMap Text Attribute -> Parser [ValidationMessage]
    validate obj = execWriterT $ sequenceA_
        [ validateGithub
        , costCenterValidate
        , ibanValidate
        , loginValidate
        , extEndDateMissing
        , attributeMissing "email" EmailMissing
        , attributeMissing "employment_type" EmploymentTypeMissing
        , attributeObjectMissing "department" TribeMissing
        , attributeObjectMissing "office" OfficeMissing
        , dynamicAttributeMissing "Work phone" PhoneMissing
        , dynamicAttributeMissing "Primary role" RoleMissing
        ]
      where
        validateGithub :: WriterT [ValidationMessage] Parser ()
        validateGithub = do
            githubText <- lift (parseDynamicAttribute obj "Github")
            case match (githubRegexp <|> pure "") githubText of
                Nothing -> tell [GithubInvalid githubText]
                Just _ -> pure ()

        isSomeText :: Text -> Maybe Text
        isSomeText = match (T.pack <$> some anySym :: RE' Text)

        checkAttributeName :: Text -> ValidationMessage -> WriterT [ValidationMessage] Parser ()
        checkAttributeName val msg = case isSomeText val of
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
              _       -> pure ()

        -- | Attribute should be fetchable with parseDynamicAttribute and error
        -- message should be a constant value
        dynamicAttributeMissing :: Text -> ValidationMessage -> WriterT [ValidationMessage] Parser ()
        dynamicAttributeMissing attrName errMsg = do
            attribute <- lift (parseDynamicAttribute obj attrName)
            case attribute of
                Array _  -> tell [errMsg]
                String a -> checkAttributeName a errMsg
                a        -> lift (typeMismatch (show attrName) a)

        costCenterValidate :: WriterT [ValidationMessage] Parser ()
        costCenterValidate = do
          (Array cost) <- lift (parseAttribute obj "cost_centers")
          case toList cost of
              [] -> tell [CostCenterMissing]
              xs -> if length xs > 1
                  then tell [CostCenterMultiple (map textShow xs)] -- TODO: Test this case
                  else pure ()

        ibanValidate :: WriterT [ValidationMessage] Parser ()
        ibanValidate = do
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

        extEndDateMissing :: WriterT [ValidationMessage] Parser()
        extEndDateMissing = do
            eType <- lift (parseAttribute obj "employment_type")
            case employmentTypeFromText eType of
                Just External -> checkEndDate
                _             -> pure ()
          where
            checkEndDate = do
              eDate <- lift (parseAttribute obj "contract_end_date")
              case eDate of
                  Null     -> tell [ExternalEndDateMissing]
                  String d -> checkAttributeName d ExternalEndDateMissing
                  _        -> pure ()

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
