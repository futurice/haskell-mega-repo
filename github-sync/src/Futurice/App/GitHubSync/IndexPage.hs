{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.GitHubSync.IndexPage (indexPage) where

import Control.Lens              (contains, filtered)
import Data.Set.Lens             (setOf)
import Data.Map.Lens             (toMapOf)
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Prelude ()

import Futurice.App.GitHubSync.Config (Pinned (..))

import qualified GitHub   as GH
import qualified Personio as P

indexPage
    :: Day
    -> Pinned
    -> [GH.User]
    -> [GH.Invitation]
    -> [P.Employee]
    -> HtmlPage "index"
indexPage today (Pin pinned) githubs githubInvs personios = page_ "GitHub sync" $ do
    fullRow_ $ h1_ "Personio ⇒ GitHub sync"

    fullRow_ $ h2_ "Only in GitHub, not in Personio"
    fullRow_ $ i_ "People in GitHub organisation, not mentioned in Personio"
    fullRow_ $ do
        table_ $ do
            thead_ $ tr_ $ do
                td_ mempty
                td_ "Username"
                td_ "Real name"
                td_ $ "Personio" >> sup_ "?"
                td_ $ "Name" >> sup_ "?"
                td_ $ "FUM" >> sup_ "?"
                td_ $ "Contract end date"  >> sup_ "?"

            tbody_ $ for_ githubs $ \u -> do
                let login = GH.userLogin u
                unless (personioLogins ^. contains login) $ tr_ $ do
                    td_ $ checkbox_ False []
                    td_ $ toHtml $ GH.userLogin u
                    td_ $ maybe "" toHtml $ GH.userName u

                    case personioMap ^? ix login of
                        Nothing -> td_ mempty >> td_ mempty >> td_ mempty >> td_ mempty
                        Just e -> do
                            td_ $ toHtml $ e ^. P.employeeId
                            td_ $ toHtml $ e ^. P.employeeFullname
                            td_ $ traverse_ toHtml $ e ^. P.employeeLogin
                            td_ $ traverse_ (toHtml . show) $ e ^. P.employeeEndDate

        div_ [ class_ "button-group" ] $
            button_ [ class_ "button alert"] "Remove"

    fullRow_ $ h2_ "Pending invitations"
    fullRow_ $
        table_ $ do
            thead_ $ tr_ $ do
                td_ "Username"
                td_ "Email"
                td_ "Invited at"
                td_ $ "Personio" >> sup_ "?"
                td_ $ "Name" >> sup_ "?"
                td_ $ "FUM" >> sup_ "?"
                td_ $ "Contract end date"  >> sup_ "?"

            tbody_ $ for_ githubInvs $ \i -> do
                tr_ $ do
                    td_ $ traverse_ toHtml $ GH.invitationLogin i
                    td_ $ traverse_ (toHtml . show) $ GH.invitationEmail i
                    td_ $ toHtml . show $  GH.invitationCreatedAt i

                    case GH.invitationLogin i >>= \login -> personioMap ^? ix login of
                        Nothing -> td_ mempty >> td_ mempty >> td_ mempty >> td_ mempty
                        Just e -> do
                            td_ $ toHtml $ e ^. P.employeeId
                            td_ $ toHtml $ e ^. P.employeeFullname
                            td_ $ traverse_ toHtml $ e ^. P.employeeLogin
                            td_ $ traverse_ (toHtml . show) $ e ^. P.employeeEndDate

    fullRow_ $ h2_ "Not in GitHub, only in Personio"
    fullRow_ $ i_ "People with GitHub information in Personio, but not added to GitHub"
    fullRow_ $ do
        table_ $ do
            thead_ $ tr_ $ do
                td_ mempty
                td_ "Personio"
                td_ "Name"
                td_ "GitHub"
            tbody_ $ for_ personios $ \e ->
                for_ (e ^. P.employeeGithub) $ \glogin ->
                    when (P.employeeIsActive today e && not (githubLogins ^. contains glogin)) $ tr_ $ do
                        td_ $ checkbox_ False []
                        td_ $ toHtml $ e ^. P.employeeId
                        td_ $ toHtml $ e ^. P.employeeFullname
                        td_ $ toHtml glogin

        div_ [ class_ "button-group" ] $
            button_ [ class_ "button warning"] "Add"
  where
    githubLogins :: Set (GH.Name GH.User)
    githubLogins = setOf (folded . getter GH.userLogin) githubs

    personioLogins :: Set (GH.Name GH.User)
    personioLogins = setOf (folded . filtered (P.employeeIsActive today) . P.employeeGithub . _Just) personios
        -- add pinned users to personio set, so we don't remove them
        <> setOf folded pinned

    personioMap :: Map (GH.Name GH.User) P.Employee
    personioMap = toMapOf (folded . getter f . _Just . ifolded) personios
      where
        f e = (,e) <$> e ^. P.employeeGithub
