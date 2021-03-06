{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Okta.Types
  ( module Okta.Types
  , module Okta.Types.Group
  , module Okta.Types.GroupInfo
  , module Okta.Types.App
  ) where

import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types                        (Parser)
import Data.Char                               (toLower)
import Futurice.Company                        (Country)
import Futurice.Constants                      (oktaAdminPublicUrl)
import Futurice.Email
import Futurice.EnvConfig
import Futurice.Generics
import Futurice.Prelude
import Lucid                                   (a_, class_, href_)
import Prelude ()
import Text.PrettyPrint.ANSI.Leijen.AnsiPretty (AnsiPretty (..))

import Okta.Types.App
import Okta.Types.Group
import Okta.Types.GroupInfo

import qualified Data.Map as Map
import qualified FUM
import qualified Personio as P

groupInfo :: OktaJSON
groupInfo = $(makeRelativeToProject "okta-groups.json" >>= embedFromJSON (Proxy :: Proxy OktaJSON))

groupMap :: Map GroupName GroupInfo
groupMap = Map.fromList $ (\g -> (giName g, g)) <$> ojGroups groupInfo

slackAppId :: OktaAppId
slackAppId = ojSlackAppId groupInfo

data OktaCfg = OktaCfg
    { oktaToken   :: !Text
    , oktaBaseUrl :: !Text
    } deriving Show

instance Configure OktaCfg where
    configure = OktaCfg
        <$> envVar "OKTA_SERVICE_TOKEN"
        <*> envVar "OKTA_BASEURL"

data Cfg = Cfg
    { oktaServiceToken :: !OktaCfg
    , manager          :: !Manager
    }

class HasOktaCfg a where
    oktaCfg :: Lens' a OktaCfg

instance HasOktaCfg Cfg where
    oktaCfg f cfg = fmap (\newToken -> cfg { oktaServiceToken = newToken}) (f (oktaServiceToken cfg))

class HasHttpManager a where
    httpManager :: Lens' a Manager

instance HasHttpManager Cfg where
    httpManager f cfg = fmap (\newm -> cfg { manager = newm }) (f (manager cfg))

data Status = Active
            | Provisioned
            | Staged
            | Other Text
            deriving (Eq, Show, Generic, NFData)

instance FromJSON Status where
    parseJSON = let go "ACTIVE"      = pure Active
                    go "PROVISIONED" = pure Provisioned
                    go "STAGED"      = pure Staged
                    go t             = pure $ Other t
                in withText "Status" go

instance ToJSON Status where
    toJSON Active      = toJSON ("ACTIVE" :: Text)
    toJSON Provisioned = toJSON ("PROVISIONED" :: Text)
    toJSON Staged      = toJSON ("STAGED" :: Text)
    toJSON (Other t)   = toJSON t

instance AnsiPretty Status where ansiPretty = ansiPretty . show

data Nationality = Native
                 | NonNative
                 deriving (Eq, Show, Generic, NFData)

instance ToJSON Nationality where
    toJSON Native = "Native"
    toJSON NonNative = "Non-native"

instance FromJSON Nationality where
    parseJSON = withText "nationality" $ \nationality ->
      case nationality of
        "Native" -> pure Native
        "Non-native" -> pure NonNative
        _ -> fail "Can't parse given string"

instance AnsiPretty Nationality where
    ansiPretty = ansiPretty . show

data Profile = Profile
    { _profileFirstName        :: !Text
    , _profileLastName         :: !Text
    , _profileMobilePhone      :: !(Maybe Text)
    , _profileSecondEmail      :: !(Maybe Text)
    , _profileLogin            :: !Text
    , _profileEmail            :: !Text
    , _profileEmployeeNumber   :: !(Maybe String)
    , _profileTribe            :: !(Maybe Text)
    , _profileOffice           :: !(Maybe Text)
    , _profileEmploymentType   :: !(Maybe Text)
    , _profileGender           :: !(Maybe Text)
    , _profileCountry          :: !(Maybe Country)
    , _profileRole             :: !(Maybe Text)
    , _profileStartDate        :: !(Maybe Day)
    , _profileManager          :: !(Maybe Email)
    , _profileFumUsername      :: !(Maybe FUM.Login)
    , _profileTerminationDate  :: !(Maybe Day)
    , _profileSeparationReason :: !(Maybe Text)
    , _profileBirthday         :: !(Maybe Day)
    , _profileCompetenceHome   :: !(Maybe Text)
    , _profileMatrixSupervisor :: !(Maybe Email)
    , _profileClientAccount    :: !(Maybe Text)
    , _profileCareerLevel      :: !(Maybe Int)
    , _profileDisplayName      :: !(Maybe Text)
    , _profileNationality      :: !(Maybe Nationality)
    } deriving (Eq, Show, GhcGeneric, SopGeneric, HasDatatypeInfo, NFData)
      deriving (ToJSON) via (Sopica Profile)

makeLenses ''Profile

instance FromJSON Profile where
    parseJSON = withObject "Profile" $ \o ->
      Profile
      <$> o .: "firstName"
      <*> o .: "lastName"
      <*> o .: "mobilePhone"
      <*> o .: "secondEmail"
      <*> o .: "login"
      <*> o .: "email"
      <*> o .:? "employeeNumber"
      <*> o .:? "tribe"
      <*> o .:? "office"
      <*> o .:? "employmentType"
      <*> o .:? "gender"
      <*> o .:? "country"
      <*> o .:? "role"
      <*> o .:? "start_date"
      <*> o .:? "manager"
      <*> (o .:? "fum_username" <|> pure Nothing)
      <*> (o .:? "terminationDate" <|> pure Nothing)
      <*> o .:? "separation_reason"
      <*> o .:? "birthday"
      <*> o .:? "competenceHome"
      <*> o .:? "matrixSupervisor"
      <*> o .:? "clientAccount"
      <*> o .:? "careerLevel"
      <*> o .:? "displayName"
      <*> o .:? "nationality"

instance AnsiPretty Profile

data GithubProfile = GithubProfile
    { githubProfileGivenName  :: !Text
    , githubProfileEmail      :: !Text
    , githubProfileFamilyName :: !Text
    , githubProfileEmailType  :: !Text
    } deriving (Show, GhcGeneric, SopGeneric, HasDatatypeInfo, NFData)
      deriving (ToJSON, FromJSON) via (Sopica GithubProfile)

instance AnsiPretty GithubProfile

newtype OktaId = OktaId Text
    deriving (Eq, Ord, Generic)
    deriving anyclass (NFData, AnsiPretty, Hashable)
    deriving newtype (FromJSON, ToJSON, Show)

instance ToHtml OktaId where
    toHtmlRaw = toHtml
    toHtml (OktaId oid) = a_ [ class_ "okta", href_ ( oktaAdminPublicUrl <> "/admin/user/profile/view/" <> oid)] (toHtml oid)

data User = User
    { _userId      :: !OktaId
    , _userStatus  :: !Status
    , _userCreated :: !UTCTime
    , _userActive  :: !(Maybe UTCTime)
    , _userProfile :: !Profile
    } deriving (Eq, Show, GhcGeneric, SopGeneric, HasDatatypeInfo, NFData)
      deriving (ToJSON, FromJSON) via (Sopica User)

makeLenses ''User

instance AnsiPretty User

data SimpleUser = SimpleUser
    { _simpleUserId         :: !OktaId
    , _simpleUserFirstName  :: !Text
    , _simpleUserSecondName :: !Text
    , _simpleUserStatus     :: !Status
    } deriving (Generic)

instance AnsiPretty SimpleUser

simpleUser :: User -> SimpleUser
simpleUser user =
    SimpleUser
    { _simpleUserId         = user ^. userId
    , _simpleUserFirstName  = user ^. userProfile . profileFirstName
    , _simpleUserSecondName = user ^. userProfile . profileLastName
    , _simpleUserStatus     = user ^. userStatus
    }

data AppUser a = AppUser
    { appUserId          :: !OktaId
    , appUserStatus      :: !Status
    , appUserCreated     :: !UTCTime
    , appUserActive      :: !(Maybe UTCTime)
    , appUserProfile     :: !a
    , appUserCredentials :: !(Maybe AppCredentials)
    } deriving (Show, GhcGeneric, SopGeneric, HasDatatypeInfo, NFData)

cleanLabel :: String -> String
cleanLabel s =
    let s' = drop 7 s
    in case s' of
      (x : xs) -> toLower x : xs
      xs       -> xs

instance (ToJSON a) => ToJSON (AppUser a) where
    toEncoding = genericToEncoding $ defaultOptions { fieldLabelModifier = cleanLabel }

instance (FromJSON a) => FromJSON (AppUser a) where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = cleanLabel }

instance (AnsiPretty a) => AnsiPretty (AppUser a)

data SlackProfile = SlackProfile
    { _spDisplayName    :: !(Maybe Text)
    , _spSlackUsername  :: !Text
    , _spEmail          :: !Text
    } deriving (Show, GhcGeneric, SopGeneric, HasDatatypeInfo, NFData)
      deriving (ToJSON, FromJSON) via (Sopica SlackProfile)

instance AnsiPretty SlackProfile

data AppLink = AppLink
    { alId            :: !Text
    , alAppName       :: !Text
    , alLabel         :: !Text
    , alLogoUrl       :: !Text
    , alAppInstanceId :: !OktaAppId
    } deriving (Show, GhcGeneric, SopGeneric, HasDatatypeInfo, NFData)
      deriving (ToJSON, FromJSON) via (Sopica AppLink)

instance AnsiPretty AppLink

data GroupType = OktaGroup
               | AppGroup
               | BuiltIn
               deriving (Eq, Show, Generic, NFData)

instance AnsiPretty GroupType where ansiPretty = ansiPretty . show

groupFromText :: Text -> Maybe GroupType
groupFromText "OKTA_GROUP" = Just OktaGroup
groupFromText "APP_GROUP"  = Just AppGroup
groupFromText "BUILT_IN"   = Just BuiltIn
groupFromText _            = Nothing

groupToText :: GroupType -> Text
groupToText OktaGroup = "OKTA_GROUP"
groupToText AppGroup  = "APP_GROUP"
groupToText BuiltIn   = "BUILT_IN"

instance FromJSON GroupType where
    parseJSON x = do
        t <- parseJSON x
        maybe empty pure (groupFromText t)

instance ToJSON GroupType where
    toJSON = toJSON . groupToText

data GroupProfile = GroupProfile
    { profileName        :: !Text
    , profileDescription :: !(Maybe Text)
    } deriving (Show, GhcGeneric, SopGeneric, HasDatatypeInfo, NFData)
      deriving (ToJSON, FromJSON) via (Sopica GroupProfile)

instance AnsiPretty GroupProfile

data Group = Group
    { groupId      :: !Text
    , groupType    :: !GroupType
    , groupProfile :: !GroupProfile
    } deriving (Show, GhcGeneric, SopGeneric, HasDatatypeInfo, NFData)
      deriving (ToJSON, FromJSON) via (Sopica Group)

instance AnsiPretty Group

data Hide = Hide
    { hideIOS :: !Bool
    , hideWeb :: !Bool
    } deriving (Show,GhcGeneric, SopGeneric, HasDatatypeInfo)
      deriving (FromJSON) via (Sopica Hide)

instance AnsiPretty Hide

data Visibility = Visibility
    { visAutoSubmitToolbar :: !Bool
    , visHide              :: !Hide
    } deriving (Show,GhcGeneric, SopGeneric, HasDatatypeInfo)
      deriving (FromJSON) via (Sopica Visibility)

instance AnsiPretty Visibility

data AppCredentials = AppCredentials
    { credUserName :: !Text
    } deriving (Show,GhcGeneric, SopGeneric, HasDatatypeInfo, NFData)

instance FromJSON AppCredentials where
    parseJSON = withObject "Credentials" $ \cred ->
      AppCredentials <$> (cred .: "userName")

instance ToJSON AppCredentials where
    toJSON (AppCredentials username) = toJSON username

instance AnsiPretty AppCredentials

data Links = Links
    { linkLogoUrls :: ![Text]
    } deriving (Show, Generic)

instance AnsiPretty Links

instance FromJSON Links where
    parseJSON = withObject "links" $ \obj -> do
      logoLinks <- (obj .: "logo" :: Parser Array)
      pure $ Links $ fmap (^. key "href" . _String) $ toList logoLinks

data App = App
    { appId          :: !Text
    , appName        :: !Text
    , appLabel       :: !Text
    , appStatus      :: !Status
    , appVisibility  :: !Visibility
    , app_links      :: !Links
    } deriving (Show,GhcGeneric, SopGeneric, HasDatatypeInfo)
      deriving (FromJSON) via (Sopica App)

instance AnsiPretty App

newtype OktaError = OktaError String deriving Show

instance Exception OktaError

data NewUser = NewUser
    { newUserProfile  :: !NewUserProfile
    , newUserGroupIds :: ![OktaGroupId]
    } deriving (Eq, Show, Hashable, GhcGeneric, SopGeneric, HasDatatypeInfo)
      deriving ToJSON via Sopica NewUser

data NewUserProfile = NewUserProfile
    { nuFirstName      :: !Text
    , nuLastName       :: !Text
    , nuLogin          :: !Text
    , nuEmail          :: !Text
    , nuSecondEmail    :: !Text
    , nuPersonioNumber :: !P.EmployeeId
    , nuGithubUsername :: !Text
    } deriving (Eq, Show, GhcGeneric, SopGeneric, HasDatatypeInfo, Hashable)

instance ToJSON NewUserProfile where
    toJSON NewUserProfile{..} = object
        [ "firstName"       .= nuFirstName
        , "lastName"        .= nuLastName
        , "login"           .= nuLogin
        , "email"           .= nuEmail
        , "secondEmail"     .= nuSecondEmail
        , "employeeNumber"  .= nuPersonioNumber -- this will probably change
        , "github_username" .= nuGithubUsername
        ]
