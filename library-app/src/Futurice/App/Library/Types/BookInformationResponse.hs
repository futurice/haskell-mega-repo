{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
module Futurice.App.Library.Types.BookInformationResponse where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.Library.Types.Library
import Futurice.App.Library.Types.Loanable

data BookInformationResponse = BookInformationResponse
    { _id          :: !LoanableId
    , _title       :: !Text
    , _ISBN        :: !Text
    , _author      :: !Text
    , _publisher   :: !Text
    , _published   :: !Int
    , _cover       :: !Text
    , _amazonLink  :: !Text
    , _library     :: !Library
    } deriving  (Eq, Ord, Show, Typeable, Generic)

deriveGeneric ''BookInformationResponse

deriveVia [t| ToJSON BookInformationResponse `Via` Sopica BookInformationResponse |]

instance ToSchema BookInformationResponse
