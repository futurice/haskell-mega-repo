{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module FUM.Types where

import Futurice.Prelude
import Prelude ()

import Data.Aeson.Compat
import Data.Aeson.Types
       (FromJSONKey (..), ToJSONKey (..), fromJSONKeyCoerce, toJSONKeyText)

import qualified Data.Maybe.Strict as S

-------------------------------------------------------------------------------
-- Authentication token
-------------------------------------------------------------------------------

-- | Authentication token
newtype AuthToken = AuthToken { _getAuthToken :: Text }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

makeLenses ''AuthToken
instance Hashable AuthToken
instance NFData AuthToken

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
-- User name
-------------------------------------------------------------------------------

-- | FUM user name
newtype UserName = UserName { _getUserName :: Text }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

makeLenses ''UserName
instance Hashable UserName
instance NFData UserName

instance FromJSON UserName where
    parseJSON = withText "FUM UserName" $ pure . UserName

instance ToJSON UserName where
    toJSON = toJSON . _getUserName

instance ToJSONKey UserName where
    toJSONKey = toJSONKeyText _getUserName

instance FromJSONKey UserName where
    fromJSONKey = fromJSONKeyCoerce

-------------------------------------------------------------------------------
-- List name
-------------------------------------------------------------------------------

-- | FUM user list
newtype ListName = ListName { _getListName :: Text }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

makeLenses ''ListName
instance Hashable ListName
instance NFData ListName

instance FromJSON ListName where
    parseJSON = withText "FUM ListName" $ pure . ListName

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
    { _userName     :: !UserName
    , _userFirst    :: !Text
    , _userLast     :: !Text
    , _userTitle    :: !(S.Maybe Text)
    , _userGithub   :: !(S.Maybe Text)
    , _userFlowdock :: !(S.Maybe Int)
    , _userEmail    :: !(S.Maybe Text) -- can be empty?
    , _userPhone1   :: !(S.Maybe Text)
    , _userPhone2   :: !(S.Maybe Text)
    , _userStatus   :: !UserStatus
    , _userImageUrl :: !(S.Maybe Text)
    , _userThumbUrl :: !(S.Maybe Text)
    , _userBadgeUrl :: !(S.Maybe Text)
    }
    deriving (Eq, Ord, Show, Read, Typeable, Generic)

makeLenses ''User
instance Hashable User
instance NFData User

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

-------------------------------------------------------------------------------
-- Utilities for aeson
-------------------------------------------------------------------------------

(.:??) :: FromJSON a => Object -> Text -> Parser (S.Maybe a)
obj .:?? key = view strict <$> obj .:? key

emptyToNothing :: Text -> Maybe Text
emptyToNothing t
    | isn't _Empty t = Just t
    | otherwise      = Nothing
