{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
module Okta.Types where

import Futurice.EnvConfig
import Futurice.Generics
import Futurice.Prelude
import Prelude ()
import Data.Aeson.Types (withText)

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
            deriving Show

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

data Profile = Profile
    { profileFirstName   :: !Text
    , profileLastName    :: !Text
    , profileMobilePhone :: !(Maybe Text)
    , profileSecondEmail :: !(Maybe Text)
    , profileLogin       :: !Text
    , profileEmail       :: !Text
    } deriving (Show, GhcGeneric, SopGeneric, HasDatatypeInfo)
      deriving (ToJSON, FromJSON) via (Sopica Profile)

data User = User
    { userId      :: !Text
    , userStatus  :: !Status
    , userCreated :: !UTCTime
    , userActive  :: !(Maybe UTCTime)
    , userProfile :: !Profile
    } deriving (Show, GhcGeneric, SopGeneric, HasDatatypeInfo)
      deriving (ToJSON, FromJSON) via (Sopica User)
