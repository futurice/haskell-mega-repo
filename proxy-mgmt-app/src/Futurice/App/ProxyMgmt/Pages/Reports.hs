{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Futurice.App.ProxyMgmt.Pages.Reports 
    (reportsPageHandler
    , chartHandler
    , chartPerUser
    , chartPerEndpoint
    , chartPerDay
    ) where

import Control.Lens               (auf, coerced, (.=))
import Data.Coerce                (Coercible, coerce)
import Data.Default.Class
import Data.Map                   (fromListWith, assocs)
import Data.List                  (groupBy)
import Data.Semigroup             (Max (..), Option (..))
import Data.Text                  (unpack)
import Data.Time.Calendar         (toGregorian)
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
import Servant.Chart              (Chart (..))

import qualified Data.Map.Strict as Map
import qualified Graphics.Rendering.Chart as C

import Futurice.App.Proxy.API (LenientEndpoint)

import Futurice.App.ProxyMgmt.API
import Futurice.App.ProxyMgmt.Commands.AddToken
import Futurice.App.ProxyMgmt.Ctx
import Futurice.App.ProxyMgmt.Markup
import Futurice.App.ProxyMgmt.Types
import Futurice.App.ProxyMgmt.Utils


isCurrMo :: Month -> AccessEntry -> Bool
isCurrMo currMonth = (== currMonth) . dayToMonth . utctDay . aeStamp

countByF :: Ord a => Month -> (AccessEntry -> a) -> [AccessEntry] -> Map a Integer
countByF cm f = fromListWith (+) . map (\x -> (f x, 1)) . filter (isCurrMo cm)


chartPerEndpoint :: Month -> [AccessEntry] -> Chart "per-endpoint"
chartPerEndpoint = pieChart "Requests per Endpoint" aeEndpoint


chartPerUser :: Month -> [AccessEntry] -> Chart "per-user"
chartPerUser = pieChart "Requests per User" aeUser


pieChart :: (Ord a, Show a) => String -> (AccessEntry -> a) -> Month -> [AccessEntry] -> Chart b
pieChart title field currMonth accessEntries = 
    Chart $ C.toRenderable layout
    
    where
        values = map (\(s, v) -> (show s, fromIntegral v)) . assocs $ countByF currMonth field accessEntries

        pitem (s,v) = C.pitem_value .~ v
                    $ C.pitem_label .~ s
                    $ C.pitem_offset .~ 0
                    $ def

        layout = C.pie_title .~ title
               $ C.pie_plot . C.pie_data .~ map pitem values
               $ def


chartPerDay :: Month -> [AccessEntry] -> Chart "per-day"
chartPerDay currMonth accessEntries = Chart $ C.toRenderable layout
    where
        values' :: [(Day, [Double])]
        values' = map (\(s, v) -> (s, [fromIntegral v])) . assocs $ countByF currMonth (utctDay . aeStamp) accessEntries

        empty = Map.fromList [ (x, [0.0]) | x <- [1..31] ]

        --values :: [(Int, [Double])]
        values = Map.fromList $ map (\(s, v) -> ((\(_,_,t) -> t) $ toGregorian s, v)) values'

        padded = Map.toList $ Map.union values empty

        titles = map (show . fst) values'
        
        line1 = C.plot_bars_titles .~ titles
              $ C.plot_bars_values .~ padded
              $ C.plot_bars_style .~ C.BarsStacked
              $ def 
        
        layout = C.layout_title .~ "Requests per Day"
               $ C.layout_plots .~ [ C.plotBars line1 ]
               $ def 

chartHandler :: (Month -> [AccessEntry] -> Chart a) -> ReaderT (Login, Ctx) IO (Chart a)
chartHandler cf = do
    (_, ctx) <- ask
    liftIO $ do
        accessEntries <- fetchAccessEntries ctx
        currMonth <- currentMonth
        pure $ cf currMonth accessEntries


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
    let userCount = countByF currMonth aeUser accessEntries
    img_ [ src_ "/chart/per-user" ]
    td_ $ ul_ $ ifor_ userCount $ \e c -> li_ $ do
        toHtml e
        " → "
        toHtml (show c)

    h2_ "Requests per Endpoint"
    let endCount = countByF currMonth aeEndpoint accessEntries
    img_ [ src_ "/chart/per-endpoint" ]
    td_ $ ul_ $ ifor_ endCount $ \e c -> li_ $ do
        toHtml e
        " → "
        toHtml (show c)

    h2_ "Requests per Day"
    let dateCount = countByF currMonth (utctDay . aeStamp) accessEntries
    img_ [ src_ "/chart/per-day" ]
    td_ $ ul_ $ ifor_ dateCount $ \e c -> li_ $ do
        toHtml (show e)
        " → "
        toHtml (show c)

