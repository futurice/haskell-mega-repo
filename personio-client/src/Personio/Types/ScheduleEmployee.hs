{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- | TODO move to personio-proxy
module Personio.Types.ScheduleEmployee where

import FUM.Types.Login
import Futurice.Email
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Personio.Types.Employee
import Personio.Types.EmployeeId

import qualified Data.Map.Strict as Map

data ScheduleEmployee = ScheduleEmployee
    { _seEmployeeId      :: !EmployeeId
    , _seLogin           :: !(Maybe Login)
    , _seName            :: !Text
    , _seEmail           :: !(Maybe Email)
    , _seSupervisorLogin :: !(Maybe Login)
    , _seSupervisorName  :: !(Maybe Text)
    , _seSupervisorEmail :: !(Maybe Email)
    , _seHrnumber        :: !(Maybe Int)
    , _seFutubuddy       :: !(Maybe Email)
    }
  deriving (Show, Typeable, Generic)

makeLenses ''ScheduleEmployee
deriveGeneric ''ScheduleEmployee

deriveVia [t| ToJSON ScheduleEmployee   `Via` Sopica ScheduleEmployee |]
deriveVia [t| FromJSON ScheduleEmployee `Via` Sopica ScheduleEmployee |]

instance ToSchema ScheduleEmployee where declareNamedSchema = sopDeclareNamedSchema

fromPersonio :: [Employee] -> [ScheduleEmployee]
fromPersonio es = map mk es
  where
    m :: Map EmployeeId Employee
    m = Map.fromList $ map (\e -> (e ^. employeeId, e)) es

    mk :: Employee -> ScheduleEmployee
    mk e = ScheduleEmployee
        { _seEmployeeId      = e ^. employeeId
        , _seLogin           = e ^. employeeLogin
        , _seName            = e ^. employeeFullname
        , _seEmail           = e ^. employeeEmail
        , _seSupervisorLogin = withSupervisor $ \s -> s ^. employeeLogin
        , _seSupervisorName  = withSupervisor $ \s -> s ^? employeeFullname
        , _seSupervisorEmail = withSupervisor $ \s -> s ^. employeeEmail
        , _seHrnumber        = e ^. employeeHRNumber
        , _seFutubuddy       = e ^. employeeFutubuddy
        }
      where
        withSupervisor :: (Employee -> Maybe a) -> Maybe a
        withSupervisor f = do
            sid <- e ^. employeeSupervisorId
            s <- m ^? ix sid
            f s
