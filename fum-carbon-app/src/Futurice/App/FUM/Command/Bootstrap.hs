{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Futurice.App.FUM.Command.Bootstrap (Bootstrap) where

import Algebra.Lattice   (bottom)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Command.Definition
import Futurice.App.FUM.Pages.Href
import Futurice.App.FUM.Types

import qualified Data.Set as Set
import qualified Personio

data Bootstrap (phase :: Phase) = Bootstrap
    { bootPersonioId :: !Personio.EmployeeId
    , bootLogin      :: !Login
    , bootUID        :: !(Phased phase () UID)
    , bootName       :: !Text
    , bootEmail      :: !Email
    , bootGroupName  :: !GroupName
    , bootGID        :: !(Phased phase () GID)
    }
  deriving (Typeable, Generic)

deriveGeneric ''Bootstrap

instance phase ~ 'Input => HasLomake (Bootstrap phase) where
    lomake _ =
        hiddenField "personioId" :*
        hiddenField "login" :*
        unitField :*
        hiddenField "name" :*
        hiddenField "email" :*
        textField   "sudoGroupName" :*
        unitField :*
        Nil

deriveVia [t| forall phase. (phase ~ 'Internal => ToJSON (Bootstrap phase))   `Via` Sopica (Bootstrap phase) |]
deriveVia [t| forall phase. (phase ~ 'Internal => FromJSON (Bootstrap phase)) `Via` Sopica (Bootstrap phase) |]

instance Command Bootstrap where
    type CommandTag Bootstrap = "bootstrap"

    internalizeCommand _now _login _rights cmd = do
        validate
        uid <- view worldNextUID
        gid <- view worldNextGID
        pure cmd
            { bootUID = uid
            , bootGID = gid
            }

    applyCommand _now _login cmd = do
        validate

        let login = bootLogin cmd
        let gname = bootGroupName cmd

        -- add user
        worldEmployees . at login ?= Employee
            { _employeeLogin        = login
            , _employeeUID          = bootUID cmd
            , _employeePersonioId   = bootPersonioId cmd
            , _employeeStatus       = StatusActive
            , _employeeName         = bootName cmd
            , _employeeEmail        = bootEmail cmd
            , _employeeEmailAliases = mempty
            , _employeeSshKeys      = mempty
            , _employeePicture      = Nothing
            , _employeePassword     = Nothing
            }

        -- make the group sudo
        worldSudoGroup ?= gname

        -- add group
        worldGroups . at gname ?= Group
            { _groupName         = gname
            , _groupGID          = bootGID cmd
            , _groupType         = GroupTypeAccess
            , _groupDescription  = "Super Users"
            , _groupEmailAliases = []
            , _groupMatch        = bottom
            , _groupEditor       = mempty
            , _groupEmployees    = Set.singleton login
            , _groupCustomers    = mempty
            }

        -- advance UID and GID
        worldNextUID %= nextUnixID
        worldNextGID %= nextUnixID

        pure $ LomakeResponseRedirect $ viewEmployeeHrefText login

-- In validation check that world is pristine, null.
validate :: (MonadReader World m, MonadError String m) => m ()
validate = do
    world <- ask
    unless (nullWorld world) $
        throwError "Trying to boostrap with non-null world!"
