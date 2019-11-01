{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Okta.Types where

import Futurice.EnvConfig
import Futurice.Generics
import Futurice.Prelude
import Prelude ()
import Data.Aeson.Types (withText)
import Text.PrettyPrint.ANSI.Leijen.AnsiPretty (AnsiPretty (..))
import Data.Aeson

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
            deriving (Show, Generic, NFData)

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

data Profile = Profile
    { profileFirstName   :: !Text
    , profileLastName    :: !Text
    , profileMobilePhone :: !(Maybe Text)
    , profileSecondEmail :: !(Maybe Text)
    , profileLogin       :: !Text
    , profileEmail       :: !Text
    } deriving (Show, GhcGeneric, SopGeneric, HasDatatypeInfo, NFData)
      deriving (ToJSON, FromJSON) via (Sopica Profile)

instance AnsiPretty Profile

data GithubProfile = GithubProfile
    { githubProfileGivenName  :: !Text
    , githubProfileEmail      :: !Text
    , githubProfileFamilyName :: !Text
    , githubProfileEmailType  :: !Text
    } deriving (Show, GhcGeneric, SopGeneric, HasDatatypeInfo, NFData)
      deriving (ToJSON, FromJSON) via (Sopica GithubProfile)

instance AnsiPretty GithubProfile

data User = User
    { userId      :: !Text
    , userStatus  :: !Status
    , userCreated :: !UTCTime
    , userActive  :: !(Maybe UTCTime)
    , userProfile :: !Profile
    } deriving (Show, GhcGeneric, SopGeneric, HasDatatypeInfo, NFData)
      deriving (ToJSON, FromJSON) via (Sopica User)

instance AnsiPretty User

data AppUser = AppUser
    { appUserId          :: !Text
    , appUserStatus      :: !Status
    , appUserCreated     :: !UTCTime
    , appUserActive      :: !(Maybe UTCTime)
    , appUserProfile     :: !GithubProfile
    , appUserCredentials :: !AppCredentials
    } deriving (Show, GhcGeneric, SopGeneric, HasDatatypeInfo, NFData)
      deriving (ToJSON, FromJSON) via (Sopica AppUser)

instance AnsiPretty AppUser

data GroupType = OktaGroup
               | AppGroup
               | BuiltIn
               deriving Show

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
        case groupFromText t of
          Just g -> pure g
          Nothing -> empty

instance ToJSON GroupType where
    toJSON = toJSON . groupToText

data GroupProfile = GroupProfile
    { profileName        :: !Text
    , profileDescription :: !(Maybe Text)
    } deriving (Show, GhcGeneric, SopGeneric, HasDatatypeInfo)
      deriving (ToJSON, FromJSON) via (Sopica GroupProfile)

instance AnsiPretty GroupProfile

data Group = Group
    { groupId      :: !Text
    , groupType    :: !GroupType
    , groupProfile :: !GroupProfile
    } deriving (Show, GhcGeneric, SopGeneric, HasDatatypeInfo)
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

data App = App
    { appId          :: !Text
    , appName        :: !Text
    , appLabel       :: !Text
    , appStatus      :: !Status
    , appVisibility  :: !Visibility
    } deriving (Show,GhcGeneric, SopGeneric, HasDatatypeInfo)
      deriving (FromJSON) via (Sopica App)

instance AnsiPretty App
