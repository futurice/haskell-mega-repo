{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.PlanMillSync.Markup (
    module Futurice.Lucid.Foundation,
    page_,
    Nav (..),
    ) where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Lucid.Navigation hiding (page_)
import Futurice.Prelude
import Prelude ()

import qualified Data.Text                 as T
import qualified Futurice.Lucid.Foundation as Lucid

data Nav
    = NavHome
    | NavOnlyInPlanmill
    | NavOnlyInPersonio
    | NavCrossEmployees
    | NavCrossSubcontractors
    | NavCrossInactive
  deriving (Eq, Ord, Enum, Bounded)

instance Navigation Nav where
    serviceTitle _ = "PlanMill Sync"

    navLink NavHome                = (href_ "/", "PlanMill Sync")
    navLink NavOnlyInPlanmill      = (href_ "/#planmill", "Only in PlanMill")
    navLink NavOnlyInPersonio      = (href_ "/#personio", "Only in Personio")
    navLink NavCrossEmployees      = (href_ "/#cross-employees", "Crosscheck: Employees")
    navLink NavCrossSubcontractors = (href_ "/#cross-subcontractors", "Crosscheck: Subcontractors")
    navLink NavCrossInactive       = (href_ "/#cross-inactive", "Crosscheck: Inactive")

    pageParams = pageParamsWithJS
        $(makeRelativeToProject "planmill-sync.js" >>= embedJS)

page_
    :: Navigation nav
    => Text -> Maybe nav -> Html () -> HtmlPage sym
page_ title nav body = do
    Lucid.page_ (title <+> serviceTitle nav) (pageParams nav) $ do
        navigation_ nav
        fullRow_ $ header_ $ h1_ title
        div_ [ class_ "row", style_ "max-width: none"] $ large_ 12 [ class_ "futu-block" ] body

(<+>) :: Text -> Text -> Text
x <+> y
    | T.null x  = y
    | T.null y  = x
    | otherwise = x <> " - " <> y
