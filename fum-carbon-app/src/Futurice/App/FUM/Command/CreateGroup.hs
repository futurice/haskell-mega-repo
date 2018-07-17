{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Futurice.App.FUM.Command.CreateGroup (CreateGroup(..)) where

import Algebra.Lattice   (bottom)
import Data.Maybe        (isJust)
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.Command.Definition
import Futurice.App.FUM.Pages.Href
import Futurice.App.FUM.Types

import qualified Data.Set as Set

data CreateGroup (phase :: Phase) = CreateGroup
    { cgName :: !GroupName
    , cgGID  :: !(Phased phase () GID)
    , cgType :: !GroupType
    }
  deriving (Typeable, Generic)

deriveGeneric ''CreateGroup

instance phase ~ 'Input => HasLomake (CreateGroup phase) where
    lomake _ =
        textField "name" :*
        unitField :*
        enumField "type" groupTypeToText :*
        -- todo description
        Nil

deriveVia [t| forall phase. (phase ~ 'Internal => ToJSON (CreateGroup phase))   `Via` Sopica (CreateGroup phase) |]
deriveVia [t| forall phase. (phase ~ 'Internal => FromJSON (CreateGroup phase)) `Via` Sopica (CreateGroup phase) |]

instance Command CreateGroup where
    type CommandTag CreateGroup = "create-group"

    internalizeCommand _now _login rights cmd = do
        requireRights RightsNormal rights
        validate cmd
        gid <- view worldNextGID
        pure cmd { cgGID = gid }

    applyCommand _now login cmd = do
        validate cmd

        let name = cgName cmd
        worldGroups . at name ?= Group
            { _groupName         = name
            , _groupGID          = cgGID cmd
            , _groupType         = cgType cmd
            , _groupDescription  = "" -- TODO
            , _groupEmailAliases = []
            , _groupMatch        = bottom
            , _groupEditor       = mempty
            , _groupEmployees    = Set.singleton login  -- creator is in the group
            , _groupCustomers    = mempty
            }

        -- make next GID one greater that any existing or current "next" GID.
        worldNextGID %= nextUnixID

        -- Redirect to the group page
        pure $ CommandResponseRedirect $ viewGroupHrefText name

validate :: (MonadReader World m, MonadError String m) => CreateGroup phase -> m ()
validate cmd = do
    let name = cgName cmd
    whenM (fmap isJust $ preview $ worldGroups . ix name) $
        throwError $ "Group with name " ++ show (groupNameToText name) ++ " already exists"
