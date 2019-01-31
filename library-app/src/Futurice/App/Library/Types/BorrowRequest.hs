{-# LANGUAGE DataKinds         #-}
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
    deriving (Show, Typeable)

deriveGeneric ''BorrowRequest

deriveVia [t| ToJSON BorrowRequest `Via` Sopica BorrowRequest |]
deriveVia [t| FromJSON BorrowRequest `Via` Sopica BorrowRequest |]

instance ToSchema BorrowRequest where declareNamedSchema = sopDeclareNamedSchema

data BorrowRequestWithUser = BorrowRequestWithUser
    { _brwuUser    :: !Login
    , _brwuRequest :: !BorrowRequest
    } deriving (Show, Typeable)

deriveGeneric ''BorrowRequestWithUser

deriveVia [t| ToJSON BorrowRequestWithUser `Via` Sopica BorrowRequestWithUser |]
deriveVia [t| FromJSON BorrowRequestWithUser `Via` Sopica BorrowRequestWithUser |]

instance ToSchema BorrowRequestWithUser where declareNamedSchema = sopDeclareNamedSchema
