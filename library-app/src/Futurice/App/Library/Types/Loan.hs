{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.Library.Types.Loan where

import Data.Aeson
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.Types.Item

import qualified Personio as P

newtype LoanId   = LoanId Int32 deriving newtype (Eq, Ord, Show, ToJSON, FromHttpApiData, FromField, ToField)

data Loan = Loan
    { _loanId            :: !LoanId
    , _loanDateLoaned    :: !Text
    , _loanInformation   :: !Item
    , _loanPerson        :: !(Maybe P.Employee)
    }
    deriving (Show, Typeable, Generic)

data ReturnedLoan = ReturnedLoan
    { _returnedLoan :: !Loan
    , _returnedDate :: !Day
    }

deriveGeneric ''LoanId
deriveGeneric ''Loan

instance ToJSON Loan where
    toJSON (Loan lid date item person) =
        object ["id"       .= lid
               , "loaned"  .= date
               , case _itemInfo item of
                   ItemBook bookinfo            -> "book"      .= bookinfo
                   ItemBoardGame boardgameinfo  -> "boardgame" .= boardgameinfo
                   ItemUnknown errortext        -> "unknown"   .= errortext
               , "loaner"  .= case person of
                                 Just p -> (p ^. P.employeeFirst) <> " " <> (p ^. P.employeeLast)
                                 Nothing -> ""
               , "library" .= _itemLibrary item
               ]

instance ToParamSchema LoanId where toParamSchema = newtypeToParamSchema
instance ToSchema LoanId where declareNamedSchema = newtypeDeclareNamedSchema
instance ToSchema Loan
