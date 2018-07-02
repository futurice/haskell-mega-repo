{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
module Futurice.App.Library.Types (
    LoanableId,
    BookId,
    BookInformation (..),
    BookInformationResponse (..),
    Loan (..),
    Loanable (..),
    LoanableInformation (..),
    LoanId,
    Library (..)
    ) where

import Database.PostgreSQL.Simple.FromField as Postgres
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Futurice.Generics
import Futurice.Prelude
import Personio.Types.EmployeeId
import Prelude ()

import Futurice.App.Library.Types.BookInformationResponse
import Futurice.App.Library.Types.Library
import Futurice.App.Library.Types.Loanable

newtype BookId   = BookId Integer deriving newtype (Eq, Ord, Show, ToJSON, FromHttpApiData, FromField, ToField)
newtype ObjectId = ObjectId Integer deriving newtype (Eq, Ord, Show, ToJSON, FromHttpApiData, FromField, ToField)
newtype LoanId   = LoanId Integer deriving newtype (Eq, Ord, Show, ToJSON, FromHttpApiData, FromField, ToField)

data BookInformation = BookInformation
    { _bookId          :: !BookId
    , _bookTitle       :: !Text
    , _bookISBN        :: !Text
    , _bookAuthor      :: !Text
    , _bookPublisher   :: !Text
    , _bookPublished   :: !Int
    , _bookCover       :: !Text
    , _bookAmazonLink  :: !Text
    , _bookLibrary     :: !Library
    }
  deriving (Eq, Ord, Show, Typeable, Generic, FromRow)

data ObjectInformation = ObjectInformation
    { _objectId    :: !ObjectId
    , _name        :: !Text
    , _description :: !Text
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

type DownloadLink = Text

data LoanableInformation = Book BookInformation
                         | Ebook BookInformation DownloadLink
                         | Object ObjectInformation
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

deriveGeneric ''BookId
deriveGeneric ''ObjectId
deriveGeneric ''BookInformation
deriveGeneric ''ObjectInformation
deriveGeneric ''LoanableInformation
deriveGeneric ''Loanable
deriveGeneric ''LoanId
deriveGeneric ''Loan

deriveVia [t| ToJSON BookInformation `Via` Sopica BookInformation |]
deriveVia [t| ToJSON ObjectInformation `Via` Sopica ObjectInformation |]
instance ToJSON LoanableInformation
deriveVia [t| ToJSON Loanable `Via` Sopica Loanable |]
deriveVia [t| ToJSON Loan `Via` Sopica Loan |]


instance ToParamSchema BookId where toParamSchema = newtypeToParamSchema
instance ToSchema BookId where declareNamedSchema = newtypeDeclareNamedSchema
instance ToParamSchema ObjectId where toParamSchema = newtypeToParamSchema
instance ToSchema ObjectId where declareNamedSchema = newtypeDeclareNamedSchema
instance ToParamSchema LoanId where toParamSchema = newtypeToParamSchema
instance ToSchema LoanId where declareNamedSchema = newtypeDeclareNamedSchema
instance ToSchema BookInformation where declareNamedSchema = sopDeclareNamedSchema
instance ToSchema ObjectInformation
instance ToSchema LoanableInformation
instance ToSchema Loanable where declareNamedSchema = sopDeclareNamedSchema
instance ToSchema Loan
