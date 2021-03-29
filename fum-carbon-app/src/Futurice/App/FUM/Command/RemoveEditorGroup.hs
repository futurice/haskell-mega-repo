{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
module Futurice.App.FUM.Command.RemoveEditorGroup (RemoveEditorGroup) where

import Control.Lens      (contains, (.=))
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.ACL
import Futurice.App.FUM.Command.Definition
import Futurice.App.FUM.Pages.Href
import Futurice.App.FUM.Types

data RemoveEditorGroup (phase :: Phase) = RemoveEditorGroup
    { regName   :: !GroupName
    , regEditor :: !GroupName
    }
  deriving (Show, Typeable, Generic)

deriveGeneric ''RemoveEditorGroup

instance phase ~ 'Input => HasLomake (RemoveEditorGroup phase) where
    lomake _ =
        dynEnumField "Group" :*
        dynEnumField "Editor group" :*
        Nil

deriving via Sopica (RemoveEditorGroup phase) instance (phase ~ 'Internal) => ToJSON (RemoveEditorGroup phase)
deriving via Sopica (RemoveEditorGroup phase) instance (phase ~ 'Internal) => FromJSON (RemoveEditorGroup phase)
--deriveVia [t| forall phase. (phase ~ 'Internal => ToJSON (RemoveEditorGroup phase))   `Via` Sopica (RemoveEditorGroup phase) |]
--deriveVia [t| forall phase. (phase ~ 'Internal => FromJSON (RemoveEditorGroup phase)) `Via` Sopica (RemoveEditorGroup phase) |]

instance Command RemoveEditorGroup where
    type CommandTag RemoveEditorGroup = "remove-editor-group"

    internalizeCommand _now login rights cmd = do
        requireRights RightsIT rights
        validate login cmd
        pure (coerce cmd)

    applyCommand _now login cmd = do
        validate login cmd

        let name = regName cmd
        let editor = regEditor cmd
        worldGroups . ix name . groupEditor . contains editor .= False

        pure $ CommandResponseRedirect $ viewGroupHrefText name

validate
    :: (MonadReader World m, MonadError String m)
    => Login -> RemoveEditorGroup phase -> m ()
validate _login cmd = do
    let name = regName cmd
    let editor = regEditor cmd

    unlessExists (worldGroups . ix name) $
        throwError $ "Group doesn't exist " ++ show (groupNameToText name)

    unlessExists (worldGroups . ix editor) $
        throwError $ "Group doesn't exist " ++ show (groupNameToText editor)

    unlessExists (worldGroups . ix name . groupEditor . ix editor) $
        throwError $ show editor ++ " is not an editor of " ++ show name
