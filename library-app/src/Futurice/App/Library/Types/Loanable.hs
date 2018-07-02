{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Futurice.App.Library.Types.Loanable where

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

newtype LoanableId = LoanableId Integer deriving newtype (Eq, Ord, Show, ToJSON, FromHttpApiData, FromField, ToField)

data LoanableType = BookType | EbookType | ObjectType

deriveGeneric ''LoanableId

instance ToParamSchema LoanableId where toParamSchema = newtypeToParamSchema
instance ToSchema LoanableId where declareNamedSchema = newtypeDeclareNamedSchema
