module Power.Class where

import Power.Types
import Power.Request

class Monad m => MonadPower m where
    powerReq :: Req a ->  m a

powerPeople :: MonadPower m => m [Person]
powerPeople = powerReq ReqPeople
