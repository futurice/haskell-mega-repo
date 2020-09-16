{-# LANGUAGE DerivingVia #-}
module Futurice.App.OktaProxy.Types where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

data AppResponse = AppResponse
    { appResLabel      :: !Text
    , appResImageUrl   :: !(Maybe Text)
    } deriving (Eq, Ord, SopGeneric, GhcGeneric, ToSchema, HasDatatypeInfo)
      deriving (FromJSON, ToJSON) via (Sopica AppResponse)
