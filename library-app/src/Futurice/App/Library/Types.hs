{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
module Futurice.App.Library.Types (
    LoanableId,
    BookId,
    BookInformation (..),
    BookInformationId,
    BookInformationResponse (..),
    BorrowRequest (..),
    Loan (..),
    Loanable (..),
    LoanableInformation (..),
    LoanId,
    Library (..)
    ) where

import Database.PostgreSQL.Simple.FromField as Postgres
import Database.PostgreSQL.Simple.ToField
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.Types.BookInformation
import Futurice.App.Library.Types.BookInformationResponse
import Futurice.App.Library.Types.BorrowRequest
import Futurice.App.Library.Types.Library
import Futurice.App.Library.Types.Loanable

newtype ObjectId = ObjectId Integer deriving newtype (Eq, Ord, Show, ToJSON, FromHttpApiData, FromField, ToField)
newtype LoanId   = LoanId Integer deriving newtype (Eq, Ord, Show, ToJSON, FromHttpApiData, FromField, ToField)

data ObjectInformation = ObjectInformation
    { _objectId    :: !ObjectId
    , _name        :: !Text
    , _description :: !Text
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

type DownloadLink = Text

data LoanableInformation = Book BookId BookInformation
                         | Ebook BookInformation DownloadLink
                         | Object ObjectId ObjectInformation
                         | NotFound Text
   deriving (Eq, Ord, Show, Typeable, Generic)

data Loanable = Loanable
    { _loanableInformation :: !LoanableInformation
    , _loanableLibrary     :: !Library
    }
    deriving (Show, Typeable, Generic)

data Loan = Loan
    { _loanId            :: !LoanId
    , _dateLoaned        :: !Text
    , _loanedInformation :: !Loanable
    , _loanPerson        :: !Text
    }
    deriving (Show, Typeable, Generic)

deriveGeneric ''ObjectId
deriveGeneric ''ObjectInformation
deriveGeneric ''LoanableInformation
deriveGeneric ''Loanable
deriveGeneric ''LoanId
deriveGeneric ''Loan

deriveVia [t| ToJSON ObjectInformation `Via` Sopica ObjectInformation |]
instance ToJSON LoanableInformation
deriveVia [t| ToJSON Loanable `Via` Sopica Loanable |]
deriveVia [t| ToJSON Loan `Via` Sopica Loan |]

instance ToParamSchema ObjectId where toParamSchema = newtypeToParamSchema
instance ToSchema ObjectId where declareNamedSchema = newtypeDeclareNamedSchema
instance ToParamSchema LoanId where toParamSchema = newtypeToParamSchema
instance ToSchema LoanId where declareNamedSchema = newtypeDeclareNamedSchema
instance ToSchema ObjectInformation
instance ToSchema LoanableInformation
instance ToSchema Loanable where declareNamedSchema = sopDeclareNamedSchema
instance ToSchema Loan
