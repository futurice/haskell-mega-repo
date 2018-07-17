{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.FUM.Command.AddEmployeeToGroup (AddEmployeeToGroup) where

import Control.Lens      (contains, (.=))
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.ACL
import Futurice.App.FUM.Command.Definition
import Futurice.App.FUM.Types

data AddEmployeeToGroup (phase :: Phase) = AddEmployeeToGroup
    { aegName  :: !GroupName
    , aegLogin :: !Login
    }
  deriving (Show, Typeable, Generic)

deriveGeneric ''AddEmployeeToGroup

instance phase ~ 'Input => HasLomake (AddEmployeeToGroup phase) where
    lomake _ =
        dynEnumField "Group" :*
        dynEnumField "Employee" :*
        Nil

deriveVia [t| forall phase. (phase ~ 'Internal => ToJSON (AddEmployeeToGroup phase))   `Via` Sopica (AddEmployeeToGroup phase) |]
deriveVia [t| forall phase. (phase ~ 'Internal => FromJSON (AddEmployeeToGroup phase)) `Via` Sopica (AddEmployeeToGroup phase) |]

instance Command AddEmployeeToGroup where
    type CommandTag AddEmployeeToGroup = "add-employee-to-group"

    internalizeCommand _now login rights cmd = do
        requireRights RightsNormal rights
        validate login cmd
        pure (coerce cmd)

    applyCommand _now login cmd = do
        validate login cmd

        let name = aegName cmd
        let addLogin = aegLogin cmd

        worldGroups . ix name . groupEmployees . contains addLogin .= True

        pure CommandResponseReload

validate
    :: (MonadReader World m, MonadError String m)
    => Login -> AddEmployeeToGroup phase -> m ()
validate login cmd = do
    let name = aegName cmd
    let addLogin = aegLogin cmd

    unlessExists (worldGroups . ix name) $
        throwError $ "Group doesn't exist " ++ show (groupNameToText name)

    unlessExists (worldEmployees . ix addLogin) $
        throwError $ "Employee doesn't exist " ++ show (loginToText addLogin)

    unlessM (canEditGroup login name) $
        throwError $ "You cannot edit group " ++ show (groupNameToText name)
