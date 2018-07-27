{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Checklist.Pages.HelpAppliance (helpAppliancePage) where

import Prelude ()
import Futurice.Prelude
import Futurice.Lucid.Foundation

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types
import Futurice.App.Checklist.Types.TaskAppliance (parseTaskAppliance)

helpAppliancePage
    :: World       -- ^ the world
    -> AuthUser    -- ^ logged in user
    -> HtmlPage "appliance-help"
helpAppliancePage _world authUser = checklistPage_ "Help" [] authUser (Just NavMore) $ do
    -- Table
    subheader_ "Examples"
    row_ $ large_ 12 $ ul_ $ do
        renderExample "helsinki or tampere" "Employees in Finland"
        renderExample "external" "Externals"
        renderExample "not external" "Not externals"
        renderExample "not (helsinki or tampere)" "Employees not in Finland"
        renderExample "not helsinki and not tampere" "Another way to say that"
        renderExample "not not not not helsinki" "you can combine expressions as you wish"
        renderExample "not helsinki or external" "..."
        renderExample "helsinki or tampere and external" "but remember to use parentheses in complex cases"
        renderExample "helsinki or (tampere and external)" "because and binds tighter"
        renderExample "(helsinki or tampere) and external" "but it's not always what you want!"

    subheader_ "Contract types"
    row_ $ large_ 12 $ ul_ $
        for_ [minBound .. maxBound ] $ \l -> li_ $ toHtml (TAContractType l)

    subheader_ "Office locations"
    row_ $ large_ 12 $ ul_ $ 
        for_ [minBound .. maxBound ] $ \l -> li_ $ toHtml (TALocation l)

 where
    renderExample :: Monad m => Text -> Text -> HtmlT m ()
    renderExample str help = case parseTaskAppliance str of
        Left err -> li_ $ span_ [ class_ "error" ] $ toHtml err
        Right app -> li_ $ do
            code_ $ toHtml str
            " = "
            toHtml app
            " ... "
            i_ $ toHtml help
