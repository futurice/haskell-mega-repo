{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DerivingVia       #-}
module Futurice.App.ProxyMgmt.Commands.UpdatePolicy where

import FUM.Types.Login
import Futurice.Generics
import Futurice.Lomake
import Futurice.Postgres
import Futurice.Prelude
import Prelude ()

import Futurice.App.Proxy.API
import Futurice.App.ProxyMgmt.Ctx
import Futurice.App.ProxyMgmt.Types
import Futurice.App.ProxyMgmt.Utils

data UpdatePolicy = UpdatePolicy
    { updatePolicyUser   :: !Login
    , updatePolicyPolicy :: !PolicyName
    }
  deriving (Show, Typeable, GhcGeneric)
  deriving anyclass (SopGeneric, HasDatatypeInfo)
  deriving (ToJSON, FromJSON) via (Sopica UpdatePolicy)