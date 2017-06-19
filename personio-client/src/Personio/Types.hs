{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
module Personio.Types where

import Control.Monad.Writer
import Data.Aeson.Compat
import Data.Aeson.Internal (JSONPathElement (Key), (<?>))
import Data.Aeson.Types    (FromJSON1 (..), explicitParseField, parseJSON1, typeMismatch, Value)
import Data.Time           (zonedTimeToLocalTime)
import Futurice.Aeson
import Futurice.EnvConfig
import Futurice.Generics
import Futurice.IdMap      (HasKey (..))
import Futurice.Prelude
import Prelude ()
import Text.Regex.Applicative.Text (RE', string, match, anySym)

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
    { _employeeId           :: !EmployeeId
    , _employeeFirst        :: !Text
    , _employeeLast         :: !Text
    , _employeeHireDate     :: !(Maybe Day)
    , _employeeEndDate      :: !(Maybe Day)
    , _employeeRole         :: !Text
    , _employeeEmail        :: !Text
    , _employeePhone        :: !Text
    , _employeeSupervisorId :: !(Maybe EmployeeId)
    , _employeeLogin        :: !(Maybe Text) -- TODO 4 or 5 lowercase letters `isLower` not good,
    , _employeeTribe        :: !(Maybe Text)
    , _employeeOffice       :: !(Maybe Text)
    , _employeeCostCenter   :: !(Maybe Text) -- exactly 1
    , _employeeGithub       :: !(Maybe Text)
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
        <*> pure (Just "foo") -- TODO: implement me
        <*> fmap getName (parseAttribute obj "department")
        <*> fmap getName (parseAttribute obj "office")
        <*> fmap getName (parseAttribute obj "cost_centers")
        <*> fmap getGithubUsername (parseDynamicAttribute obj "Github")
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
        (Array xs) ->  case toList xs of
            []    -> pure (NamedAttribute Nothing)
            (x:_) -> p x  -- take first attribute.
        _ -> p v
      where
        p = withObjectDump "NamedAttribute" $ \obj ->
            NamedAttribute . Just <$> ((obj .: "attributes") >>= (.: "name"))

newtype GithubUsername = GithubUsername { getGithubUsername :: Maybe Text }

instance FromJSON GithubUsername where
    parseJSON = withText "Github" (pure . GithubUsername . match regexp)
      where
        regexp :: RE' Text
        regexp = string "https://github.com/" *> (T.pack <$> some anySym)

-------------------------------------------------------------------------------
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
  deriving (Eq, Ord, Show, Typeable, Generic)


instance ToJSON ValidationMessage
instance FromJSON ValidationMessage

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
        , attributeMissing "email" EmailMissing
        , attributeObjectMissing "department" TribeMissing
        , attributeObjectMissing "office" OfficeMissing
        , dynamicAttributeMissing "Work phone" PhoneMissing
        , dynamicAttributeMissing "Primary role" RoleMissing
        ]
      where
        validateGithub :: WriterT [ValidationMessage] Parser ()
        validateGithub = do
            githubText <- lift (parseDynamicAttribute obj "Github")
            case match regexp githubText of
                Nothing -> tell [GithubInvalid githubText]
                Just _ -> pure ()
          where
            regexp :: RE' Text
            regexp = string "https://github.com/" *> (T.pack <$> some anySym)
                <|> pure "" -- or it can be empty!

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
              Array _ -> tell [errMsg] -- | Should not be an array!
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

isValidIBAN :: Text -> Bool
isValidIBAN iban = isValidLength preparsed && isValid parsed
  where
      preparsed = T.filter (/= ' ') $ T.append (T.drop 4 iban) (T.take 4 iban)
      parsed    = parse preparsed

      isValid :: Maybe Integer -> Bool
      isValid ib = case ib of
          Nothing -> False -- If parameter is false, when parse-function failed to read strint to integer due to invalid characters
          Just i  -> i `mod` 97 == 1

      charToNum :: Char -> [Char]
      charToNum c =
          let letters = T.pack ['A'..'Z']
          in case T.findIndex (c ==) letters of
              Nothing -> [c]
              Just n  -> show (n + 10) -- A == 10, B == 11 etc.

      charsToNums = map charToNum . T.unpack

      parse :: Text -> Maybe Integer
      parse ib = readMaybe (concat $ charsToNums ib) :: Maybe Integer -- Returns Nothing in case of invalid characters

      -- | Shortest 15 from Norway, longest 31 from Malta
      -- https://en.wikipedia.org/wiki/International_Bank_Account_Number#IBAN_formats_by_country
      isValidLength :: Text -> Bool
      isValidLength i = T.length i >= 15 && T.length i <= 31
