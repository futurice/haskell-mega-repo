{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futurice.App.FUM.Command.ChangeGroupMatch (ChangeGroupMatch) where

import Control.Lens      ((.=))
import Futurice.Generics
import Futurice.Prelude
import Prelude ()

import Futurice.App.FUM.ACL
import Futurice.App.FUM.Command.Definition
import Futurice.App.FUM.Pages.Href
import Futurice.App.FUM.Types

data ChangeGroupMatch (phase :: Phase) = ChangeGroupMatch
    { cgmName  :: !GroupName
    , cgmMatch :: !GroupMatch
    }
  deriving (Show, Typeable, Generic)

deriveGeneric ''ChangeGroupMatch

instance phase ~ 'Input => HasLomake (ChangeGroupMatch phase) where
    lomake _ =
        dynEnumField "Group" :*
        textField "Employee match" :*
        Nil

deriveVia [t| forall phase. (phase ~ 'Internal => ToJSON (ChangeGroupMatch phase))   `Via` Sopica (ChangeGroupMatch phase) |]
deriveVia [t| forall phase. (phase ~ 'Internal => FromJSON (ChangeGroupMatch phase)) `Via` Sopica (ChangeGroupMatch phase) |]

instance Command ChangeGroupMatch where
    type CommandTag ChangeGroupMatch = "change-group-match"

    internalizeCommand _now login rights cmd = do
        requireRights RightsIT rights
        validate login cmd
        pure (coerce cmd)

    applyCommand _now login cmd = do
        validate login cmd

        let name = cgmName cmd
        let gm   = cgmMatch cmd

        worldGroups . ix name . groupMatch .= gm

        pure $ CommandResponseRedirect $ viewGroupHrefText name

validate
    :: (MonadReader World m, MonadError String m)
    => Login -> ChangeGroupMatch phase -> m ()
validate _login cmd = do
    let name = cgmName cmd

    unlessExists (worldGroups . ix name) $
        throwError $ "Group doesn't exist " ++ show (groupNameToText name)
