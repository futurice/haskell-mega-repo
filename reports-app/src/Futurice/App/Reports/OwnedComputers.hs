{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Futurice.App.Reports.OwnedComputers where

import Database.PostgreSQL.Simple (FromRow)
import Futurice.Generics
import Futurice.Postgres          (HasPostgresPool, safePoolQuery_)
import Futurice.Prelude
import Prelude ()

import qualified Data.Map as Map
import qualified Personio as P

-------------------------------------------------------------------------------
-- Fetch
-------------------------------------------------------------------------------

data Computer = Computer
    { name    :: !Text
    , serial  :: !(Maybe Text)
    , ownerId :: !Int
    } deriving (Generic, FromRow, ToJSON, ToSchema)

data User = User
    { userId     :: !Int
    , employeeId :: !P.EmployeeId
    } deriving (Generic, FromRow)

inventoryComputerQuery :: HasPostgresPool ctx => ctx -> LogT IO [Computer]
inventoryComputerQuery ctx = safePoolQuery_ ctx
    "select name, serial, owner_id from common_computer"

inventoryUserQuery :: HasPostgresPool ctx => ctx -> LogT IO [User]
inventoryUserQuery ctx = safePoolQuery_ ctx
    "select id, personio_id from common_user where personio_id IS NOT NULL"

userComputers :: HasPostgresPool ctx => ctx -> LogT IO (Map P.EmployeeId [Text])
userComputers ctx = do
    computers <- inventoryComputerQuery ctx
    users <- inventoryUserQuery ctx
    let userMap = Map.fromList $ fmap (\u -> (userId u, employeeId u)) users
    let ownerIdToEmployeeId oid = Map.lookup oid userMap
    let res = Map.fromListWith (<>) $ catMaybes $ fmap (\c -> (, [name c]) <$> ownerIdToEmployeeId (ownerId c)) computers
    pure res
