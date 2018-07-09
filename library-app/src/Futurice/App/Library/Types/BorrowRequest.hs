{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
module Futurice.App.Library.Types.BorrowRequest where

import Futurice.Generics
import Futurice.Prelude
import Personio.Types.EmployeeId
import Prelude ()

import Futurice.App.Library.Types.BookInformation
import Futurice.App.Library.Types.Library

data BorrowRequest = BorrowRequest
    { _borrowRequester :: !EmployeeId
    , _borrowBook      :: !BookInformationId
    , _borrowLibrary   :: !Library
    }
    deriving (Show, Typeable)

deriveGeneric ''BorrowRequest

deriveVia [t| FromJSON BorrowRequest `Via` Sopica BorrowRequest |]

instance ToSchema BorrowRequest where declareNamedSchema = sopDeclareNamedSchema
