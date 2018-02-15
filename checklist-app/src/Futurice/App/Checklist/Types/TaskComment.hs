{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module Futurice.App.Checklist.Types.TaskComment where

import Futurice.Generics
import Futurice.Prelude
import Prelude ()

newtype TaskComment = TaskComment Text
  deriving (Eq, Show)
  deriving newtype (ToJSON, FromJSON, ToHtml, Arbitrary)

deriveGeneric ''TaskComment
