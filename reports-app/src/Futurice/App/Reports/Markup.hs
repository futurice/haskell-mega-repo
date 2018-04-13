{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Reports.Markup (indexPage) where

import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()
import Generics.SOP (hpure, hsequenceK, hcmap)

import Futurice.App.Reports.API

links :: NP Proxy Reports
links = hpure Proxy

makeLink :: forall r m. (RClass r, Monad m) => Proxy r -> K (HtmlT m ()) r
makeLink _ = K $ li_ $ do
    a_ [ href_ $ "/" <> textVal ppath ] $ toHtml $ textVal pname
    " ["
    -- TODO: we'd like to use ".json", but we need type-level sybmol concatenation
    -- https://ghc.haskell.org/trac/ghc/ticket/12162
    a_ [ href_ $ "/" <> textVal ppath <> "/json" ] $ "JSON"
    "] ["
    a_ [ href_ $ "/" <> textVal ppath <> "/csv" ] $ "CSV"
    "]"
  where
    ppath = Proxy :: Proxy (RPath r)
    pname = Proxy :: Proxy (RName r)

indexPage :: HtmlPage "index"
indexPage = page_ "Reports" $ do
    row_ $ large_ 12 $ h1_ "Reports"

    fullRow_ $ h2_ "Dashdo"
    fullRow_ $ do
        a_ [ href_ "/dashdo/" ] "Dashdo dashboards"
        ", including missing-hours, balances, and other"

    fullRow_ $ h2_ "Charts"
    fullRow_ $ ul_ $ do
        li_ $ a_ [ href_ "/charts/career-length" ] "Distribution of career lengths over time, absolute"
        li_ $ a_ [ href_ "/charts/career-length-relative" ] "Distribution of career length over time, relative"

    fullRow_ $ h2_ "Graphs"
    fullRow_ $ ul_ $ do
        li_ $ a_ [ href_ "/graphs/supervisors" ] "Supervisor graph"

    fullRow_ $ h2_ "Tables"
    fullRow_ $ ul_ $ do
        void $ hsequenceK $ hcmap (Proxy :: Proxy RClass) makeLink links

    fullRow_ $ h2_ "Integrations for Power"
    fullRow_ $ ul_ $ do
        li_ $ a_ [ href_ "/power/users" ] "Users"
        li_ $ a_ [ href_ "/power/projects" ] "Projects"
        li_ $ a_ [ href_ "/power/absences" ] "Absences"
