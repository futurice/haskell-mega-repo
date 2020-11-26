{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.Constants (
    avatarPublicUrl,
    avatarPublicUrlStr,
    fumPublicUrl,
    fumPublicUrlStr,
    personioPublicUrl,
    personioPublicUrlStr,
    planmillPublicUrl,
    planmillPublicUrlStr,
    servicePublicUrl,
    supportEmailHtml,
    competenceMap,
    powerPublicUrlStr,
    powerPublicUrl,
    oktaAdminPublicUrl,
    oktaAdminPublicUrlStr
    ) where

import Data.Functor.Rep  (index)
import Futurice.Prelude
import Futurice.Services
import Lucid             (HtmlT, a_, href_, toHtml)
import Prelude ()

import qualified Futurice.Constants.Internal as I

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

constants :: I.Constants
constants = $(makeRelativeToProject "constants.json" >>= embedFromJSON (Proxy :: Proxy I.Constants))

-------------------------------------------------------------------------------
-- Services
-------------------------------------------------------------------------------

servicePublicUrl :: Service -> Text
servicePublicUrl = index (I.publicUrls constants)

avatarPublicUrl :: Text
avatarPublicUrl = servicePublicUrl AvatarService

avatarPublicUrlStr :: String
avatarPublicUrlStr = avatarPublicUrl ^. unpacked

fumPublicUrl :: Text
fumPublicUrl = servicePublicUrl FumService

fumPublicUrlStr :: String
fumPublicUrlStr = fumPublicUrl ^. unpacked

personioPublicUrl :: Text
personioPublicUrl = servicePublicUrl PersonioService

personioPublicUrlStr :: String
personioPublicUrlStr = personioPublicUrl ^. unpacked

planmillPublicUrl :: Text
planmillPublicUrl = servicePublicUrl PlanmillService

planmillPublicUrlStr :: String
planmillPublicUrlStr = planmillPublicUrl ^. unpacked

powerPublicUrl :: Text
powerPublicUrl = servicePublicUrl PowerService

powerPublicUrlStr :: String
powerPublicUrlStr = powerPublicUrl ^. unpacked

oktaAdminPublicUrl :: Text
oktaAdminPublicUrl = servicePublicUrl OktaAdminService

oktaAdminPublicUrlStr :: String
oktaAdminPublicUrlStr = oktaAdminPublicUrl ^. unpacked

-------------------------------------------------------------------------------
-- Other constants
-------------------------------------------------------------------------------

supportEmailHtml :: Monad m => HtmlT m ()
supportEmailHtml = a_ [ href_ $ "mailto:" <> addr ] $ toHtml addr where
    addr = I.supportEmailAddr constants

competenceMap :: Map Text Text
competenceMap = I.competenceMap constants
