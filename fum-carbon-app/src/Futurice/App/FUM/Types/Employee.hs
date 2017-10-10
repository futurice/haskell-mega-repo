{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.FUM.Types.Employee where

import FUM.Types.Login
import Futurice.Email
import Futurice.IdMap      (HasKey (..))
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Types.Identifier
import Futurice.App.FUM.Types.Status
import Futurice.App.FUM.Types.UnixID

import qualified Personio as P

-- | TODO:
type SshKey = Text

-- | We always have /some/ picture of the employee.
type Picture = Text

-- | Employee: person
--
-- Name etc. comes from Personio.
--
-- /TODO:/ store name?
data Employee = Employee
    { _employeeLogin          :: !Login
    , _employeeUID            :: !UID
    , _employeePersonioId     :: !P.EmployeeId     -- ^ @123@, provides information to names, contract data etc.
    , _employeeStatus         :: !Status           -- ^ "futurice status", importantly not directly the google status.
    , _employeeName           :: !Text             -- ^ name, periodically sync'd from personio
    , _employeeEmail          :: !Email
    , _employeeEmailAliases   :: !(Set Email)
    , _employeeSshKeys        :: ![SshKey]
    , _employeePicture        :: !(Maybe Picture)
    , _employeePasswordExp    :: !UTCTime          -- ^ password expiration date, does LDAP expires?
--    , _employeePassword :: FORMAT?       -- ^ will make LDAP server easy, SHA-512
    }
  deriving (Eq, Ord, Show, Typeable, Generic)

makeLenses ''Employee
deriveGeneric ''Employee

instance NFData Employee

instance HasKey Employee where
    type Key Employee = Login
    key = employeeLogin

instance Entity Employee  where entityName _ = "FUM.Employee"