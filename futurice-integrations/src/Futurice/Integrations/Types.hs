{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.Integrations.Types where

import Futurice.Prelude
import Prelude ()

import Futurice.Generics
import Futurice.Report.Columns (ToColumns)
import Futurice.Tribe          (Tribe)

-- | Employee information often used in reports
--
-- /TODO/ lensify
data Employee = Employee
    { employeeName     :: !Text
    , employeeTribe    :: !Tribe
    , employeeContract :: !Text
    }
  deriving stock (Eq, Show, Typeable, Generic)
  deriving anyclass (NFData)

deriveGeneric ''Employee
deriving anyclass instance  ToColumns Employee
instance ToSchema Employee where declareNamedSchema = sopDeclareNamedSchema
deriveVia [t| ToJSON Employee   `Via` Sopica Employee |]
deriveVia [t| FromJSON Employee `Via` Sopica Employee |]
