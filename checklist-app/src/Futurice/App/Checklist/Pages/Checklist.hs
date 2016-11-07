{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.Checklist (checklistPage) where

import Futurice.Prelude
import Prelude ()
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Types
import Futurice.App.Checklist.Markup

checklistPage
    :: World
    -> UUID -- todo
    -> HtmlPage "checklist"
checklistPage _ _ = nonAuthorizedPage
