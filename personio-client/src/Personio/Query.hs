{-# LANGUAGE GADTs #-}
module Personio.Query where

import Futurice.Prelude
import Prelude ()

import Personio.Types

-- For more general API calls than Request
data Query a where
    QueryEmployee :: EmployeeId -> Query Employee
    QueryEmployeePicture :: EmployeeId -> Query ByteString
