{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.FUM.Command.RemoveEmployeeFromGroup (RemoveEmployeeFromGroup) where

import Control.Lens      (contains, (.=))
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.ACL
import Futurice.App.FUM.Command.Definition
import Futurice.App.FUM.Types

data RemoveEmployeeFromGroup (phase :: Phase) = RemoveEmployeeFromGroup
    { regName  :: !GroupName
    , regLogin :: !Login
    }
  deriving (Show, Typeable, Generic)

deriveGeneric ''RemoveEmployeeFromGroup

instance phase ~ 'Input => HasLomake (RemoveEmployeeFromGroup phase) where
    lomake _ =
        dynEnumField "Group" :*
        dynEnumField "Employee" :*
        Nil

deriveVia [t| forall phase. (phase ~ 'Internal => ToJSON (RemoveEmployeeFromGroup phase))   `Via` Sopica (RemoveEmployeeFromGroup phase) |]
deriveVia [t| forall phase. (phase ~ 'Internal => FromJSON (RemoveEmployeeFromGroup phase)) `Via` Sopica (RemoveEmployeeFromGroup phase) |]

instance Command RemoveEmployeeFromGroup where
    type CommandTag RemoveEmployeeFromGroup = "remove-employee-from-group"

    internalizeCommand _now login rights cmd = do
        requireRights RightsNormal rights
        validate login cmd
        pure (coerce cmd)

    applyCommand _now login' cmd = do
        validate login' cmd

        let name = regName cmd
        let login = regLogin cmd

        worldGroups . ix name . groupEmployees . contains login .= False

        pure LomakeResponseReload

validate
    :: (MonadReader World m, MonadError String m)
    => Login -> RemoveEmployeeFromGroup phase -> m ()
validate login' cmd = do
    let name = regName cmd
    let login = regLogin cmd

    unlessExists (worldGroups . ix name) $
        throwError $ "Group doesn't exist " ++ show (groupNameToText name)

    unlessExists (worldGroups . ix name . groupEmployees . ix login) $
        throwError $ "Employee isn't in the group"

    unlessM (canEditGroup login' name) $
        throwError $ "You cannot edit group " ++ show (groupNameToText name)

    -- TODO: more checks?
