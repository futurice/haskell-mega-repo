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
import Futurice.App.Library.Types.ItemType

import qualified Personio as P

-------------------------------------------------------------------------------
-- LoanId
-------------------------------------------------------------------------------

newtype LoanId = LoanId Int32
  deriving newtype (Eq, Ord, Show, ToJSON, FromHttpApiData, ToHttpApiData, FromField, ToField)

deriveGeneric ''LoanId
instance ToParamSchema LoanId where toParamSchema = newtypeToParamSchema
instance ToSchema LoanId where declareNamedSchema = newtypeDeclareNamedSchema

-------------------------------------------------------------------------------
-- LoanStatus
-------------------------------------------------------------------------------

data LoanStatus = Loaned | NotLoaned

-------------------------------------------------------------------------------
-- Loan
-------------------------------------------------------------------------------

data Loan = Loan
    { _loanId            :: !LoanId
    , _loanDateLoaned    :: !Text
    , _loanInformation   :: !Item
    , _loanPerson        :: !(Maybe P.Employee)
    }
    deriving (Show, Typeable, Generic)

makeLenses ''Loan
deriveGeneric ''Loan

instance ToJSON Loan where
    toJSON (Loan lid date item person) = object
        ["id"       .= lid
        , "loaned"  .= date
        , case item ^. itemInfo of
            MkSome (ItemBook bookinfo)            -> "book"      .= bookinfo
            MkSome (ItemBoardGame boardgameinfo)  -> "boardgame" .= boardgameinfo
        , "loaner"  .= case person of
            Just p -> (p ^. P.employeeFirst) <> " " <> (p ^. P.employeeLast)
            Nothing -> ""
        , "library" .= _itemLibrary item
        ]

-- | Not correct instance.
instance ToSchema Loan

-------------------------------------------------------------------------------
-- ReturnedLoan
-------------------------------------------------------------------------------

data ReturnedLoan = ReturnedLoan
    { _returnedLoan :: !Loan
    , _returnedDate :: !Day
    }


