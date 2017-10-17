{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.FUM.Command.RemoveEmailFromEmployee (RemoveEmailFromEmployee) where

import Control.Lens      (contains, (.=))
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.ACL
import Futurice.App.FUM.Command.Definition
import Futurice.App.FUM.Pages.Href
import Futurice.App.FUM.Types

data RemoveEmailFromEmployee (phase :: Phase) = RemoveEmailFromEmployee
    { reeLogin :: !Login
    , reeEmail :: !Email
    }
  deriving (Show, Typeable, Generic)

deriveGeneric ''RemoveEmailFromEmployee

instance phase ~ 'Input => HasLomake (RemoveEmailFromEmployee phase) where
    lomake _ =
        hiddenField "Login" :*
        hiddenField "Email address" :*
        Nil

instance phase ~ 'Internal => ToJSON (RemoveEmailFromEmployee phase) where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance phase ~ 'Internal => FromJSON (RemoveEmailFromEmployee phase) where
    parseJSON = sopParseJSON

instance Command RemoveEmailFromEmployee where
    type CommandTag RemoveEmailFromEmployee = "remove-email-from-employee"

    internalizeCommand _now login rights cmd = do
        requireRights RightsNormal rights
        validate login cmd
        pure (coerce cmd)

    applyCommand _now login' cmd = do
        validate login' cmd
        let login = reeLogin cmd

        worldEmployees . ix login . employeeEmailAliases . contains (reeEmail cmd) .= False

        pure $ LomakeResponseRedirect $ viewEmployeeHrefText login

validate
    :: (MonadReader World m, MonadError String m)
    => Login -> RemoveEmailFromEmployee phase -> m ()
validate login' cmd = do
    let login = reeLogin cmd

    -- check rights
    unlessM (canEditEmployee login' login) $
        throwError $ "You cannot edit user " ++ show (loginToText login)

    -- check that users has an email, this validates also that user exists.
    unlessExists (worldEmployees . ix login . employeeEmailAliases . ix (reeEmail cmd)) $
        throwError $ "Employee doesn't have that email alias"
