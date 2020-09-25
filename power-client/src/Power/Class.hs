module Power.Class where

import Futurice.Prelude
import Prelude ()

import Power.Request
import Power.Types

class Monad m => MonadPower m where
    powerReq :: Req a ->  m a

powerPeople :: MonadPower m => m [Person]
powerPeople = powerReq ReqPeople

powerAllocations :: MonadPower m => m [Allocation]
powerAllocations = powerReq $ ReqAllocation Nothing Nothing

powerAllocationsByDate :: MonadPower m => Maybe Day -> Maybe Day -> m [Allocation]
powerAllocationsByDate startDate endDate = powerReq $ ReqAllocation startDate endDate

powerCustomers :: MonadPower m => m [Customer]
powerCustomers = powerReq ReqCustomer

powerProjects :: MonadPower m => m [Project]
powerProjects = powerReq ReqProject

powerProjectMapping :: MonadPower m => m [ProjectMapping]
powerProjectMapping = powerReq ReqProjectMapping
