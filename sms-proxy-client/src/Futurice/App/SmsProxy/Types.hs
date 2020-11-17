{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE InstanceSigs     #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}
module Futurice.App.SmsProxy.Types where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

data Res = Res
    { _resTo     :: !Text
    , _resStatus :: !Text
    }
  deriving (Show, Generic)

data Req = Req
    { _reqTo   :: !Text
    , _reqText :: !Text
    }
  deriving (Show, Generic)

instance NFData Res
instance NFData Req

-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------

makeLenses ''Res
makeLenses ''Req

deriveGeneric ''Res
deriveGeneric ''Req

deriveVia [t| ToJSON Res `Via` Sopica Res |]
deriveVia [t| ToJSON Req `Via` Sopica Req |]

deriveVia [t| FromJSON Res `Via` Sopica Res |]
deriveVia [t| FromJSON Req `Via` Sopica Req |]

instance ToSchema Res where declareNamedSchema = sopDeclareNamedSchema
instance ToSchema Req where declareNamedSchema = sopDeclareNamedSchema
