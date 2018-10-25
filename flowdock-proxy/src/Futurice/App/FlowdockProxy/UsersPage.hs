{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.FlowdockProxy.UsersPage (usersPage) where

import Control.Lens     (forOf_)
import Futurice.Email   (Email, emailFromText)
import Futurice.Prelude
import Prelude ()

import qualified Chat.Flowdock.REST as FD
import qualified Data.Map.Strict    as Map
import qualified Personio           as P

import Futurice.App.FlowdockProxy.Markup

usersPage
    :: Day
    -> FD.Organisation
    -> [FD.Organisation]
    -> [P.Employee]
    ->  HtmlPage "users-page"
usersPage today org orgs ps = page_ "Users page" (Just NavHome) $ do
    h2_ $ "Only in " <> org ^. FD.orgName <> " Flowdock"
    ul_ $ do
        li_ "i.e. cannot connect to a person in personio"
        li_ "We try to connect by Flowdock id, if known"
        li_ "Or by @futurice.com email"
    table_ $ do
        thead_ $ tr_ $ do
            th_ "ID"
            th_ "Name"
            th_ "Email"
            th_ "Personio"
            th_ "Personio end date"

        tbody_ $ forOf_ (FD.orgUsers . folded) org $ \u -> do
            let byId    = Map.lookup (u ^. FD.userId) personioUsersById
                byEmail = do
                    email <- emailFromText $ u ^. FD.userEmail
                    Map.lookup email personioUsersByEmail
            case byId <|> byEmail of
                Nothing -> tr_ $ do
                    td_ $ toHtmlIdent $ u ^. FD.userId
                    td_ $ toHtml $ u ^. FD.userName
                    td_ $ toHtml $ u ^. FD.userEmail
                    td_ mempty
                    td_ mempty
                Just p | not (P.employeeIsActive today p) -> tr_ $ do
                    td_ $ toHtmlIdent $ u ^. FD.userId
                    td_ $ toHtml $ u ^. FD.userName
                    td_ $ toHtml $ u ^. FD.userEmail
                    td_ $ toHtml $ p ^. P.employeeId
                    td_ $ traverse_ (toHtml . show) $ p ^. P.employeeEndDate
                _ -> mempty

    for_ orgs $ \o -> do
        h2_ $ "Only in " <> o ^. FD.orgName <> " Flowdock"
        table_ $ do
            thead_ $ tr_ $ do
                th_ "ID"
                th_ "Name"
                th_ "Email"
                th_ "Personio"
                th_ "Personio end date"
            tbody_ $ forOf_ (FD.orgUsers . folded) o $ \u -> do
                let byId    = Map.lookup (u ^. FD.userId) personioUsersById
                    byEmail = do
                        email <- emailFromText $ u ^. FD.userEmail
                        Map.lookup email personioUsersByEmail
                case byId <|> byEmail of
                    -- if we don't find a user, we don't show it.
                    {-
                    Nothing -> tr_ $ do
                        td_ $ toHtmlIdent $ u ^. FD.userId
                        td_ $ toHtml $ u ^. FD.userName
                        td_ $ toHtml $ u ^. FD.userEmail
                        td_ mempty
                        td_ mempty
                    -}
                    Just p | not (P.employeeIsActive today p) -> tr_ $ do
                        td_ $ toHtmlIdent $ u ^. FD.userId
                        td_ $ toHtml $ u ^. FD.userName
                        td_ $ toHtml $ u ^. FD.userEmail
                        td_ $ toHtml $ p ^. P.employeeId
                        td_ $ traverse_ (toHtml . show) $ p ^. P.employeeEndDate
                    _ -> mempty

  where
    personioUsersById :: Map FD.UserId P.Employee
    personioUsersById = Map.fromList
        [ (i, p)
        | p <- ps
        , i <- p ^.. P.employeeFlowdock . _Just
        ]

    personioUsersByEmail :: Map Email P.Employee
    personioUsersByEmail = Map.fromList
        [ (i, p)
        | p <- ps
        , i <- p ^.. P.employeeEmail . _Just
        ]

    toHtmlIdent :: Monad m => FD.UserId -> HtmlT m ()
    toHtmlIdent ident =
        a_ [ href_ url ] $ toHtml ids
      where
        ids = textShow (FD.getIdentifier ident)
        url = mconcat
            [ "https://flowdock.com/app/private/"
            , ids
            ]



