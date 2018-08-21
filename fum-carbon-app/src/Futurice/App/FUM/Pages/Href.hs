{-# LANGUAGE OverloadedStrings #-}
-- | Safe links for Pages API
module Futurice.App.FUM.Pages.Href (
    module Futurice.App.FUM.Pages.Href,
    ) where

import FUM.Types.GroupName       (GroupName)
import FUM.Types.Login           (Login)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()
import Servant.Links             (Link, safeLink)
import Web.HttpApiData           (toUrlPiece)

import Futurice.App.FUM.API.Pages

import qualified Personio

-------------------------------------------------------------------------------
-- Index page
-------------------------------------------------------------------------------

indexPageHref_ :: Attribute
indexPageHref_ = href_ $ linkToText $ safeLink fumCarbonPagesApi
    indexPageEndpoint

fromPersonioHrefText :: Text
fromPersonioHrefText = linkToText $ safeLink fumCarbonPagesApi
    fromPersonioPageEndpoint

fromPersonioPageHref_ :: Attribute
fromPersonioPageHref_ = href_ fromPersonioHrefText

-------------------------------------------------------------------------------
-- Employees
-------------------------------------------------------------------------------

listEmployeesHref_ :: Attribute
listEmployeesHref_ = href_ $ linkToText $ safeLink fumCarbonPagesApi
    listEmployeesPageEndpoint

viewEmployeeHref_ :: Login -> Attribute
viewEmployeeHref_ = href_ . viewEmployeeHrefText

viewEmployeeHrefText :: Login -> Text
viewEmployeeHrefText login = linkToText $ safeLink fumCarbonPagesApi
    viewEmployeePageEndpoint
    login

createEmployeeHref_ :: Personio.EmployeeId -> Attribute
createEmployeeHref_ eid = href_ $ linkToText $ safeLink fumCarbonPagesApi
    createEmployeePageEndpoint
    eid

-------------------------------------------------------------------------------
-- Groups
-------------------------------------------------------------------------------

listGroupsHref_ :: Attribute
listGroupsHref_ = href_ $ linkToText $ safeLink fumCarbonPagesApi
    listGroupsPageEndpoint

viewGroupHref_ :: GroupName -> Attribute
viewGroupHref_ = href_ . viewGroupHrefText

viewGroupHrefText :: GroupName -> Text
viewGroupHrefText gn = linkToText $ safeLink fumCarbonPagesApi
    viewGroupPageEndpoint
    gn

createGroupHref_ :: Attribute
createGroupHref_ = href_ $ linkToText $ safeLink fumCarbonPagesApi
    createGroupPageEndpoint

-------------------------------------------------------------------------------
-- IT stuff
-------------------------------------------------------------------------------

summaryHref_ :: Attribute
summaryHref_ = href_ $ linkToText $ safeLink fumCarbonPagesApi
    summaryPageEndpoint

-------------------------------------------------------------------------------
-- Internals
-------------------------------------------------------------------------------

linkToText :: Link -> Text
linkToText l = "/" <> toUrlPiece l

linkToAttribute :: Link -> Attribute
linkToAttribute = href_ . linkToText
