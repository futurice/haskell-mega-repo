{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Futurice.App.FUM.Command.Bootstrap (Bootstrap (..)) where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Command.Definition
import Futurice.App.FUM.Pages.Href
import Futurice.App.FUM.Types

import qualified Personio
import qualified Data.Set as Set

data Bootstrap (phase :: Phase) = Bootstrap
    { bootPersonioId :: !Personio.EmployeeId
    , bootLogin      :: !Login
    , bootName       :: !Text
    , bootEmail      :: !Text
    , bootGroupName  :: !GroupName
    }
  deriving (Show, Typeable, Generic)

deriveGeneric ''Bootstrap

instance phase ~ 'Input => HasLomake (Bootstrap phase) where
    lomake _ =
        hiddenField "personioId" :*
        hiddenField "login" :*
        hiddenField "name" :*
        hiddenField "email" :*
        textField   "sudoGroupName" :*
        Nil

instance phase ~ 'Internal => ToJSON (Bootstrap phase) where
    toJSON = sopToJSON
    toEncoding = sopToEncoding

instance phase ~ 'Internal => FromJSON (Bootstrap phase) where
    parseJSON = sopParseJSON

instance Command Bootstrap where
    type CommandTag Bootstrap = "bootstrap"

    internalizeCommand _now _login _rights cmd = do
        validate
        pure (coerce cmd)

    applyCommand now _login cmd = do
        validate

        let login = bootLogin cmd
        let gname = bootGroupName cmd

        -- add user
        worldEmployees . at login ?= Employee
            { _employeeLogin        = login
            , _employeePersonioId   = bootPersonioId cmd
            , _employeeStatus       = StatusActive
            , _employeeName         = bootName cmd
            , _employeeEmailAliases = [ bootEmail cmd ]
            , _employeeSshKeys      = []
            , _employeePicture      = Nothing
            , _employeePasswordExp  = now  -- TODO
            }

        -- make the group sudo
        worldSudoGroup ?= gname

        -- add group
        worldGroups . at gname ?= Group
            { _groupName         = gname
            , _groupType         = GroupTypeAccess
            , _groupDescription  = "Super Users"
            , _groupEmailAliases = []
            , _groupEditor       = mempty
            , _groupEmployees    = Set.singleton login
            , _groupCustomers    = mempty
            }

        pure $ LomakeResponseRedirect $ viewEmployeeHrefText login

-- In validation check that world is pristine, null.
validate :: (MonadReader World m, MonadError String m) => m ()
validate = do
    world <- ask
    unless (nullWorld world) $
        throwError "Trying to boostrap with non-null world!"