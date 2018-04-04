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
    supportEmailHtml,
    competenceMap,
    ) where

import Futurice.Prelude
import Lucid            (HtmlT, a_, href_, toHtml)
import Prelude ()

import qualified Futurice.Constants.Internal as I

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

constants :: I.Constants
constants = $(makeRelativeToProject "constants.json" >>= embedFromJSON (Proxy :: Proxy I.Constants))

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

avatarPublicUrl :: Text
avatarPublicUrl = I.avatarPublicUrl constants

avatarPublicUrlStr :: String
avatarPublicUrlStr = avatarPublicUrl ^. unpacked

fumPublicUrl :: Text
fumPublicUrl = I.fumPublicUrl constants

fumPublicUrlStr :: String
fumPublicUrlStr = fumPublicUrl ^. unpacked

personioPublicUrl :: Text
personioPublicUrl = I.personioPublicUrl constants

personioPublicUrlStr :: String
personioPublicUrlStr = personioPublicUrl ^. unpacked

planmillPublicUrl :: Text
planmillPublicUrl = I.planmillPublicUrl constants

planmillPublicUrlStr :: String
planmillPublicUrlStr = planmillPublicUrl ^. unpacked

supportEmailHtml :: Monad m => HtmlT m ()
supportEmailHtml = a_ [ href_ $ "mailto:" <> addr ] $ toHtml addr where
    addr = I.supportEmailAddr constants

competenceMap :: Map Text Text
competenceMap = I.competenceMap constants
