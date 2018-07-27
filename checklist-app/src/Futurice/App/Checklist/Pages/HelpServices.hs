{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Checklist.Pages.HelpServices (helpServicesPage) where

import Futurice.Constants
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Futurice.Services
import Prelude ()

import Futurice.App.Checklist.Markup
import Futurice.App.Checklist.Types

import qualified Data.ByteString.Base64 as Base64

helpServicesPage
    :: World       -- ^ the world
    -> AuthUser    -- ^ logged in user
    -> HtmlPage "services-help"
helpServicesPage _world authUser = checklistPage_ "Help - Services" [] authUser (Just NavMore) $ do
    table_ $ do
        thead_ $ tr_ $ do
            th_ mempty
            th_ "Service"
            th_ "Description"
        tbody_ $ for_ [minBound .. maxBound] $ \s -> tr_ $ do
            let url = servicePublicUrl s
            let mavatarUrl = case s of
                    PersonioService -> Just "https://cdn2.personio.de/favicon-194x194.png?v=3"
                    PowerService    -> Just powerFavicon
                    FumService      -> Nothing
                    PlanmillService -> Nothing
                    _               -> Just (url <> "/favicon.ico")

            td_ $ for_ mavatarUrl $ \avatarUrl -> img_ [ src_ avatarUrl, width_ "24" ]
            td_ $ a_ [ href_ url ] $ toHtml url
            td_ $ for_ (desc s) $ \d -> " " <> i_ (toHtml d)
  where
    desc :: Service -> Maybe Text
    desc HCService           = Just "Reports for HC, including Personio validation page"
    desc GithubSyncService   = Just "Personio → GitHub sync / data validation"
    desc PlanmillSyncService = Just "Personio → PlanMill sync / data validation"
    desc _ = Nothing

powerFavicon :: Text
powerFavicon = "data:image/png;base64," <> decodeUtf8Lenient b64 where
    b64 = Base64.encode $(makeRelativeToProject "power-favicon.png" >>= embedByteString)
