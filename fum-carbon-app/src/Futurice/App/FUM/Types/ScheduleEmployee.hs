{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}
module Futurice.App.FUM.Types.ScheduleEmployee where

import FUM.Types.Login
import Futurice.Email
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import qualified Data.Map.Strict as Map
import qualified Personio        as P

data ScheduleEmployee = ScheduleEmployee
    { _seLogin           :: !(Maybe Login)
    , _seName            :: !Text
    , _seEmail           :: !(Maybe Email)
    , _seSupervisorLogin :: !(Maybe Login)
    , _seSupervisorName  :: !(Maybe Text)
    , _seSupervisorEmail :: !(Maybe Email)
    }
  deriving (Show, Typeable, Generic)

makeLenses ''ScheduleEmployee
deriveGeneric ''ScheduleEmployee

deriveVia [t| ToJSON ScheduleEmployee   `Via` Sopica ScheduleEmployee |]
deriveVia [t| FromJSON ScheduleEmployee `Via` Sopica ScheduleEmployee |]

instance ToSchema ScheduleEmployee where declareNamedSchema = sopDeclareNamedSchema

fromPersonio :: [P.Employee] -> [ScheduleEmployee]
fromPersonio es = map mk es
  where
    m :: Map P.EmployeeId P.Employee
    m = Map.fromList $ map (\e -> (e ^. P.employeeId, e)) es

    mk :: P.Employee -> ScheduleEmployee
    mk e = ScheduleEmployee
        { _seLogin           = e ^. P.employeeLogin
        , _seName            = e ^. P.employeeFullname
        , _seEmail           = e ^. P.employeeEmail
        , _seSupervisorLogin = withSupervisor $ \s -> s ^. P.employeeLogin
        , _seSupervisorName  = withSupervisor $ \s -> s ^? P.employeeFullname
        , _seSupervisorEmail = withSupervisor $ \s -> s ^. P.employeeEmail
        }
      where
        withSupervisor :: (P.Employee -> Maybe a) -> Maybe a
        withSupervisor f = do
            sid <- e ^. P.employeeSupervisorId
            s <- m ^? ix sid
            f s
