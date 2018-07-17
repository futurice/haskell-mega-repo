{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.FUM.Command.RemoveSSHKeyFromEmployee (RemoveSSHKeyFromEmployee) where

import Control.Lens      (only)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.ACL
import Futurice.App.FUM.Command.Definition
import Futurice.App.FUM.Pages.Href
import Futurice.App.FUM.Types

import qualified Data.Set as Set

data RemoveSSHKeyFromEmployee (phase :: Phase) = RemoveSSHKeyFromEmployee
    { rsheLogin :: !Login
    , rsheSSHKey :: !SSHKeyFingerprint
    }
  deriving (Show, Typeable, Generic)

deriveGeneric ''RemoveSSHKeyFromEmployee

instance phase ~ 'Input => HasLomake (RemoveSSHKeyFromEmployee phase) where
    lomake _ =
        hiddenField "Login" :*
        hiddenField "SSHKeyFingerprint" :*
        Nil

deriveVia [t| forall phase. (phase ~ 'Internal => ToJSON (RemoveSSHKeyFromEmployee phase))   `Via` Sopica (RemoveSSHKeyFromEmployee phase) |]
deriveVia [t| forall phase. (phase ~ 'Internal => FromJSON (RemoveSSHKeyFromEmployee phase)) `Via` Sopica (RemoveSSHKeyFromEmployee phase) |]

instance Command RemoveSSHKeyFromEmployee where
    type CommandTag RemoveSSHKeyFromEmployee = "remove-sshkey-from-employee"

    internalizeCommand _now login rights cmd = do
        requireRights RightsNormal rights
        validate login cmd
        pure (coerce cmd)

    applyCommand _now login' cmd = do
        validate login' cmd
        let login = rsheLogin cmd
        let fingerprint = rsheSSHKey cmd
        let predicate sshkey = _sshKeyFingerprint sshkey /= fingerprint

        worldEmployees . ix login . employeeSshKeys %= Set.filter predicate

        pure $ CommandResponseRedirect $ viewEmployeeHrefText login

validate
    :: (MonadReader World m, MonadError String m)
    => Login -> RemoveSSHKeyFromEmployee phase -> m ()
validate login' cmd = do
    let login = rsheLogin cmd
    let fingerprint = rsheSSHKey cmd

    -- check rights
    unlessM (canEditEmployee login' login) $
        throwError $ "You cannot edit user " ++ show (loginToText login)

    -- check that users has an email, this validates also that user exists.
    unlessExists (worldEmployees . ix login . employeeSshKeys . folded . sshKeyFingerprint . only fingerprint) $
        throwError $ "Employee doesn't have that SSH Key"
