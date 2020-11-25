{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
module Okta.Types.GroupInfo where

import Futurice.EnvConfig
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Okta.Types.Group

import qualified Data.Text as T

-- For group info file

data OktaJSON = OktaJSON
    { ojExternalGroup :: !GroupName,
      ojInternalGroup :: !GroupName,
      ojGroups        :: ![GroupInfo]
    } deriving (SopGeneric, GhcGeneric, HasDatatypeInfo, Lift)
      deriving (FromJSON) via Sopica OktaJSON

newtype GroupName = GroupName Text
    deriving (Eq, Ord, Generic, Lift, Show)
    deriving anyclass (HasDatatypeInfo, SopGeneric, FromJSON, ToJSON, ToSchema)

instance FromEnvVar GroupName where
    fromEnvVar = fmap GroupName . fromEnvVar

instance FromEnvVarList GroupName where
    fromEnvVarList = traverse (fmap GroupName . fromEnvVar) . map T.unpack . T.splitOn "," . T.pack

groupNameToText :: GroupName -> Text
groupNameToText (GroupName t) = t

data GroupInfo = GroupInfo
    { giId   :: !OktaGroupId
    , giName :: !GroupName
    } deriving (SopGeneric, GhcGeneric, HasDatatypeInfo, Lift)
      deriving (FromJSON) via Sopica GroupInfo
