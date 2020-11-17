{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.FUM.Command.AddSSHKeyToEmployee (AddSSHKeyToEmployee) where

import Control.Lens      (contains, (.=))
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.ACL
import Futurice.App.FUM.Command.Definition
import Futurice.App.FUM.Pages.Href
import Futurice.App.FUM.Types

data AddSSHKeyToEmployee (phase :: Phase) = AddSSHKeyToEmployee
    { askeLogin  :: !Login
    , askeSshKey :: !SSHKey
    }
  deriving (Show, Typeable, Generic)

deriveGeneric ''AddSSHKeyToEmployee

instance phase ~ 'Input => HasLomake (AddSSHKeyToEmployee phase) where
    lomake _ =
        hiddenField "Login" :*
        textField "SSH Key" :*
        Nil

deriveVia [t| forall phase. (phase ~ 'Internal => ToJSON (AddSSHKeyToEmployee phase))   `Via` Sopica (AddSSHKeyToEmployee phase) |]
deriveVia [t| forall phase. (phase ~ 'Internal => FromJSON (AddSSHKeyToEmployee phase)) `Via` Sopica (AddSSHKeyToEmployee phase) |]

instance Command AddSSHKeyToEmployee where
    type CommandTag AddSSHKeyToEmployee = "add-sshkey-to-employee"

    internalizeCommand _now login rights cmd = do
        requireRights RightsNormal rights
        validate login cmd
        pure (coerce cmd)

    applyCommand _now login' cmd = do
        validate login' cmd
        let login = askeLogin cmd

        worldEmployees . ix login . employeeSshKeys . contains (askeSshKey cmd) .= True

        pure $ CommandResponseRedirect $ viewEmployeeHrefText login

validate
    :: (MonadReader World m, MonadError String m)
    => Login -> AddSSHKeyToEmployee phase -> m ()
validate login' cmd = do
    let login = askeLogin cmd

    -- user exists
    unlessExists (worldEmployees . ix login) $
        throwError $ "Employee doesn't exist " ++ show (loginToText login)

    -- check rights
    unlessM (canEditEmployee login' login) $
        throwError $ "You cannot edit user " ++ show (loginToText login)

    -- TODO: Check that sshkeys are unique
    {-
    emails <- view worldEmails
    when (emails ^. contains (askeEmail cmd)) $
        throwError $ "Email already in use"
    -}
