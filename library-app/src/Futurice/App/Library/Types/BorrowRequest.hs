{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Library.Types.BorrowRequest where

import Data.Aeson
import FUM.Types.Login
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.Types.BookInformation
import Futurice.App.Library.Types.Library

data BorrowRequest = BorrowRequest
    { _borrowBook      :: !BookInformationId
    , _borrowLibrary   :: !Library
    }
    deriving (Show, Typeable, GhcGeneric, SopGeneric, HasDatatypeInfo)
    deriving (ToJSON, FromJSON) via (Sopica BorrowRequest)

instance ToSchema BorrowRequest where declareNamedSchema = sopDeclareNamedSchema

data BorrowRequestWithUser = BorrowRequestWithUser
    { _brwuUser    :: !Login
    , _brwuRequest :: !BorrowRequest
    } deriving (Show, Typeable, GhcGeneric, SopGeneric, HasDatatypeInfo)
      deriving (ToJSON, FromJSON) via (Sopica BorrowRequestWithUser)

instance ToSchema BorrowRequestWithUser where declareNamedSchema = sopDeclareNamedSchema
