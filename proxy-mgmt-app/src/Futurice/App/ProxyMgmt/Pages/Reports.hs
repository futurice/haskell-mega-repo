{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Futurice.App.ProxyMgmt.Pages.Reports (reportsPageHandler) where

import Control.Lens               (auf, coerced)
import Data.Coerce                (Coercible, coerce)
import Data.Map                   (fromListWith)
import Data.List                  (groupBy)
import Data.Semigroup             (Max (..), Option (..))
import Database.PostgreSQL.Simple (Only (..))
import FUM.Types.Login
import Futurice.Generics          (textualToText)
import Futurice.Lomake
import Futurice.Postgres
import Futurice.Prelude
import Futurice.Servant           (cachedIO)
import Futurice.Time.Month        (dayToMonth)
import Prelude ()
import Servant.Links              (fieldLink)

import qualified Data.Map.Strict as Map

import Futurice.App.Proxy.API (LenientEndpoint)

import Futurice.App.ProxyMgmt.API
import Futurice.App.ProxyMgmt.Commands.AddToken
import Futurice.App.ProxyMgmt.Ctx
import Futurice.App.ProxyMgmt.Markup
import Futurice.App.ProxyMgmt.Types
import Futurice.App.ProxyMgmt.Utils


reportsPageHandler :: ReaderT (Login, Ctx) IO (HtmlPage "reports")
reportsPageHandler = do
    (_, ctx) <- ask
    liftIO $ do
        policies <- fetchPolicies ctx
        tokens <- fetchTokens ctx
        accessEntries <- fetchAccessEntries ctx
        currMonth <- currentMonth
        pure $ reportsPage currMonth policies tokens accessEntries


reportsPage :: Month -> [PolicyName] -> [Token] -> [AccessEntry] -> HtmlPage "reports"
reportsPage currMonth policies tokens accessEntries = page_ "Reports" (Just NavReports) $ do
    h2_ "Requests per User"
    let userCount = countByF aeUser accessEntries
    td_ $ ul_ $ ifor_ userCount $ \e c -> li_ $ do
        toHtml e
        " → "
        toHtml (show c)

    h2_ "Requests per Endpoint"
    let endCount = countByF aeEndpoint accessEntries
    td_ $ ul_ $ ifor_ endCount $ \e c -> li_ $ do
        toHtml e
        " → "
        toHtml (show c)

    h2_ "Requests per Day"
    let dateCount = countByF (formatHumanHelsinkiTime . aeStamp) accessEntries
    td_ $ ul_ $ ifor_ dateCount $ \e c -> li_ $ do
        toHtml e
        " → "
        toHtml (show c)

    where
        isCurrMo :: AccessEntry -> Bool
        isCurrMo = (== currMonth) . dayToMonth . utctDay . aeStamp

        countByF f = fromListWith (+) . map (\x -> (f x, 1)) . filter isCurrMo