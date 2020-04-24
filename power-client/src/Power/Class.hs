module Power.Class where

import Power.Request
import Power.Types

class Monad m => MonadPower m where
    powerReq :: Req a ->  m a

powerPeople :: MonadPower m => m [Person]
powerPeople = powerReq ReqPeople

powerAllocations :: MonadPower m => m [Allocation]
powerAllocations = powerReq ReqAllocation

powerCustomers :: MonadPower m => m [Customer]
powerCustomers = powerReq ReqCustomer

powerProjects :: MonadPower m => m [Project]
powerProjects = powerReq ReqProject
