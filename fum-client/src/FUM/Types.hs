{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
module FUM.Types (
    module FUM.Types,
    module FUM.Types.Login,
    module FUM.Types.GroupName,
    ) where

import Data.Aeson.Compat
import FUM.Types.GroupName
import FUM.Types.Login
import Futurice.EnvConfig  (FromEnvVar (..))
import Futurice.IdMap      (HasKey (..))
import Futurice.Prelude
import Prelude ()

import qualified Data.Maybe.Strict as S

-------------------------------------------------------------------------------
-- Utilities for aeson
-------------------------------------------------------------------------------

-- We need this before TH-splices.

(.:??) :: FromJSON a => Object -> Text -> Parser (S.Maybe a)
obj .:?? k = view strict <$> obj .:? k

emptyToNothing :: Text -> Maybe Text
emptyToNothing t
    | isn't _Empty t = Just t
    | otherwise      = Nothing

-------------------------------------------------------------------------------
-- Authentication token
-------------------------------------------------------------------------------

-- | Authentication token
newtype AuthToken = AuthToken { _getAuthToken :: Text }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

makeLenses ''AuthToken
instance Hashable AuthToken
instance NFData AuthToken

instance IsString AuthToken where
    fromString = AuthToken . view packed

class HasAuthToken a where
    authToken :: Lens' a AuthToken

instance HasAuthToken AuthToken where
    authToken = id

instance FromJSON AuthToken where
    parseJSON = withText "FUM AuthToken" $ pure . AuthToken

-------------------------------------------------------------------------------
-- Base url
-------------------------------------------------------------------------------

-- | Base url of FUM service
newtype BaseUrl = BaseUrl { _getBaseUrl :: String }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

makeLenses ''BaseUrl
instance Hashable BaseUrl
instance NFData BaseUrl

instance IsString BaseUrl where
    fromString = BaseUrl . view packed

class HasBaseUrl a where
    baseUrl :: Lens' a BaseUrl

instance HasBaseUrl BaseUrl where
    baseUrl = id

instance FromJSON BaseUrl where
    parseJSON = withText "FUM BaseUrl" $ pure . BaseUrl . view (from packed)

-------------------------------------------------------------------------------
-- Cfg
-------------------------------------------------------------------------------

data Cfg = Cfg
    { _cfgBaseUrl   :: !BaseUrl
    , _cfgAuthToken :: !AuthToken
    }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

makeLenses ''Cfg
instance Hashable Cfg
instance NFData Cfg

instance HasAuthToken Cfg where authToken = cfgAuthToken
instance HasBaseUrl Cfg where baseUrl = cfgBaseUrl

-------------------------------------------------------------------------------
-- List name
-------------------------------------------------------------------------------

-- | FUM user list
newtype ListName = ListName { _getListName :: Text }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

makeLenses ''ListName
instance Hashable ListName
instance NFData ListName

instance IsString ListName where
    fromString = ListName . view packed

instance FromJSON ListName where
    parseJSON = withText "FUM ListName" $ pure . ListName

instance ToJSON ListName where
    toJSON = toJSON . _getListName

-------------------------------------------------------------------------------
-- User status
-------------------------------------------------------------------------------

data UserStatus
    = StatusActive
    | StatusDisabled
    | StatusDeleted
    deriving (Eq, Ord, Show, Read, Enum, Bounded, Typeable, Generic)

makePrisms ''UserStatus
instance Hashable UserStatus
instance NFData UserStatus

instance FromJSON UserStatus where
    parseJSON = withText "User status" $ \t ->
        case t of
            "active"   -> pure StatusActive
            "disabled" -> pure StatusDisabled
            "deleted"  -> pure StatusDeleted
            _          -> fail $ "Unknown status: " <> t ^. from packed

-------------------------------------------------------------------------------
-- User
-------------------------------------------------------------------------------

data User = User
    { _userName         :: !Login
    , _userFirst        :: !Text
    , _userLast         :: !Text
    , _userTitle        :: !(S.Maybe Text)
    , _userGithub       :: !(S.Maybe Text)
    , _userFlowdock     :: !(S.Maybe Int)
    , _userEmail        :: !(S.Maybe Text) -- can be empty?
    , _userPhone1       :: !(S.Maybe Text)
    , _userPhone2       :: !(S.Maybe Text)
    , _userStatus       :: !UserStatus
    , _userImageUrl     :: !(S.Maybe Text)
    , _userThumbUrl     :: !(S.Maybe Text)
    , _userBadgeUrl     :: !(S.Maybe Text)
    , _userId           :: !Int
    , _userSupervisor   :: !(S.Maybe Int)
    , _userActiveInPm   :: !Int
    , _userHrNumber     :: !(S.Maybe Text)
    , _userGoogleStatus :: !(S.Maybe Text)
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

makeLenses ''User
instance Hashable User
instance NFData User

instance HasKey User where
    type Key User = Login
    key = userName

instance FromJSON User where
    parseJSON = withObject "User object" $ \v -> User
        <$> v .: "username"
        <*> v .:? "first_name" .!= ""
        <*> v .:? "last_name" .!= ""
        <*> v .: "title"
        <*> (view strict . emptyToNothing <$> v .: "github")
        <*> v .:?? "flowdock_uid"
        <*> v .: "email"
        <*> v .: "phone1"
        <*> (view strict . (>>= emptyToNothing) <$> v .: "phone2")
        <*> v .: "status"
        <*> v .: "portrait_full_url"
        <*> v .: "portrait_thumb_url"
        <*> v .: "portrait_badge_url"
        <*> v .: "id"
        <*> v .:?? "supervisor"
        <*> v .: "active_in_planmill"
        <*> v .:?? "hr_number"
        <*> v .:?? "google_status"

userFullName :: Getter User Text
userFullName = getter $ \u -> u ^. userFirst <> " " <> u ^. userLast

-------------------------------------------------------------------------------
-- Group
-------------------------------------------------------------------------------

-- Object (fromList [("email",String "teamit@futurice.com"),("users",Array [String "epan",String "itteam",String "jvai",String "kaho",String "lekl",String "lrom",String "emkos",String "ogre",String "rlar",String "tsuo",String "thak"]),("email_aliases",Array []),("resources",Array []),("name",String "TeamIT"),("id",Number 2323.0),("description",String "It Infra Team"),("editor_group",String "TeamIT")])
data Group = Group
    { _groupName        :: !GroupName
    , _groupEmail       :: !(Maybe Text)
    , _groupDescription :: !Text
    , _groupEditor      :: !GroupName
    , _groupUsers       :: !(Vector Login)
    , _groupId          :: !Int
    -- group resources, json key: resources
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

makeLenses ''Group
instance Hashable Group
instance NFData Group

instance FromJSON Group where
    parseJSON = withObject "Group object" $ \v -> Group
        <$> v .: "name"
        <*> v .:? "email"
        <*> v .: "description"
        <*> v .: "editor_group"
        <*> v .: "users"
        <*> v .: "id"

-------------------------------------------------------------------------------
-- EnvConfig
-------------------------------------------------------------------------------

instance FromEnvVar AuthToken where
    fromEnvVar = fmap AuthToken . fromEnvVar

instance FromEnvVar BaseUrl where
    fromEnvVar = Just . BaseUrl

instance FromEnvVar ListName where
    fromEnvVar = fmap ListName . fromEnvVar
