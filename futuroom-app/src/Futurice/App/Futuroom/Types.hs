{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Futuroom.Types where

import Data.Aeson
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

newtype WeekNumber = WeekNumber Int
    deriving stock (Generic)
    deriving newtype (FromHttpApiData)
    deriving anyclass (ToParamSchema)

newtype Year = Year Int
    deriving stock (Generic)
    deriving newtype (FromHttpApiData)
    deriving anyclass (ToParamSchema)

data MeetingRoom = MeetingRoom
    { mrName     :: !Text
    , mrEmail    :: !Text
    , mrLocation :: !(Maybe Text)
    , mrCapasity :: !(Maybe Int)
    , mrFloor    :: !(Maybe Text)
    } deriving (Show, Ord, Eq, Generic, NFData)

data Reservation = Reservation
    { resStartTime :: !(Maybe UTCTime)
    , resEndTime   :: !(Maybe UTCTime)
    , resTitle     :: !(Maybe Text)
    } deriving (Show, Generic, ToJSON, ToSchema, NFData)

data ServiceAccount = ServiceAccount
    { credPrivateKey   :: !Text
    , credClientId     :: !Text
    , credClientEmail  :: !Text
    , credPrivateKeyId :: !Text
    } deriving stock (Show, GhcGeneric)
      deriving anyclass (SopGeneric, HasDatatypeInfo, ToSchema)
      deriving (FromJSON) via (Sopica ServiceAccount)

instance ToJSON ServiceAccount where
    toJSON (ServiceAccount key cid email keyid) = object [ "private_key" .= key, "client_id" .= cid, "client_email" .= email, "private_key_id" .= keyid ]
