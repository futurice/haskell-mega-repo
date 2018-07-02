{-# LANGUAGE DataKinds #-}

module Futurice.App.Library.IndexPage where

import Futurice.App.Library.Ctx

import Futurice.Lucid.Foundation (HtmlPage)
import Futurice.Prelude
import Prelude ()
import Servant

indexPage :: Ctx -> Handler (HtmlPage "indexpage")
indexPage = undefined
