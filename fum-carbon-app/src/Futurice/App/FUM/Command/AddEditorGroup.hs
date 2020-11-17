{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.FUM.Command.AddEditorGroup (AddEditorGroup) where

import Control.Lens      (contains, (.=))
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.ACL
import Futurice.App.FUM.Command.Definition
import Futurice.App.FUM.Pages.Href
import Futurice.App.FUM.Types

data AddEditorGroup (phase :: Phase) = AddEditorGroup
    { aegName   :: !GroupName
    , aegEditor :: !GroupName
    }
  deriving (Show, Typeable, Generic)

deriveGeneric ''AddEditorGroup

instance phase ~ 'Input => HasLomake (AddEditorGroup phase) where
    lomake _ =
        dynEnumField "Group" :*
        dynEnumField "Editor group" :*
        Nil

deriveVia [t| forall phase. (phase ~ 'Internal => ToJSON (AddEditorGroup phase))   `Via` Sopica (AddEditorGroup phase) |]
deriveVia [t| forall phase. (phase ~ 'Internal => FromJSON (AddEditorGroup phase)) `Via` Sopica (AddEditorGroup phase) |]

instance Command AddEditorGroup where
    type CommandTag AddEditorGroup = "add-editor-group"

    internalizeCommand _now login rights cmd = do
        requireRights RightsIT rights
        validate login cmd
        pure (coerce cmd)

    applyCommand _now login cmd = do
        validate login cmd

        let name = aegName cmd
        let editor = aegEditor cmd
        worldGroups . ix name . groupEditor . contains editor .= True

        pure $ CommandResponseRedirect $ viewGroupHrefText name

validate
    :: (MonadReader World m, MonadError String m)
    => Login -> AddEditorGroup phase -> m ()
validate _login cmd = do
    let name = aegName cmd
    let editor = aegEditor cmd

    unlessExists (worldGroups . ix name) $
        throwError $ "Group doesn't exist " ++ show (groupNameToText name)

    unlessExists (worldGroups . ix editor) $
        throwError $ "Group doesn't exist " ++ show (groupNameToText editor)

    whenExists (worldGroups . ix name . groupEditor . ix editor) $
        throwError $ show editor ++ " is already editor of " ++ show name
