{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
module Okta.Types.GroupInfo where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Okta.Types.Group

-- For group info file

data OktaJSON = OktaJSON
    { ojExternalGroup :: !Text,
      ojInternalGroup :: !Text,
      ojGroups        :: [GroupInfo]
    } deriving (SopGeneric, GhcGeneric, HasDatatypeInfo, Lift)
      deriving (FromJSON) via Sopica OktaJSON

data GroupInfo = GroupInfo
    { giId   :: !OktaGroupId
    , giName :: !Text
    } deriving (SopGeneric, GhcGeneric, HasDatatypeInfo, Lift)
      deriving (FromJSON) via Sopica GroupInfo
