{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.ProxyMgmt.IndexPage (indexPageHandler) where

import Futurice.Prelude
import Prelude ()
import FUM.Types.Login

import Futurice.App.ProxyMgmt.Ctx
import Futurice.App.ProxyMgmt.Markup

indexPageHandler :: Ctx f -> ReaderT Login IO (HtmlPage "index")
indexPageHandler _ = return $ page_ "Prox management" (Just NavIndex) $ do
    "foobar"

    "barfoo"
