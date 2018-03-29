{-# LANGUAGE DataKinds #-}
module PlanMill.Types.Inserted (
    HookInserted,
    Inserted)
    where

import Data.Aeson.Extra          (SingObject (..))
import PlanMill.Types.Hook       (Hook)
import PlanMill.Types.Identifier (Identifier)
import PlanMill.Types.Timereport (Timereport)

-- | Result type of 'addTimereport'.
type Inserted i = SingObject "id" (Identifier Timereport)

-- | Result type of 'addHook'.
type HookInserted i = SingObject "id" (Identifier Hook)
