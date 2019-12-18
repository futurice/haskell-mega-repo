{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Okta.IndexPage where

import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Okta.Markup

import qualified Data.Map as Map
import qualified Okta     as O
import qualified Personio as P

indexPage :: [P.Employee] -> [O.User] -> HtmlPage "indexpage"
indexPage employees users = page_ "Okta sync" (Just NavHome) $ do
    fullRow_ $ do
        h2_ "Active people in Personio that are not in Okta"
        sortableTable_ $ do
            thead_ $ do
                th_ "Name"
                th_ "Employment"
            tbody_ $ do
                for_ activeEmployeesNotInOkta $ \e -> tr_ $ do
                  td_ $ toHtml (e ^. P.employeeFullname)
                  td_ $ toHtml $ maybe "" show (e ^. P.employeeEmploymentType)
        h2_ "People in Okta that are not in Personio"
        sortableTable_ $ do
            thead_ $ do
                th_ "Name"
                th_ "Login"
            tbody_ $ do
                for_ oktaUsersNotInPersonio $ \u -> tr_ $ do
                    td_ $ toHtml $ (O.profileFirstName $ O.userProfile u) <> " " <> (O.profileLastName $ O.userProfile u)
                    td_ $ toHtml $ O.profileLogin $ O.userProfile u
        h2_ "All employees in Personio"
        sortableTable_ $ do
            thead_ $ do
                th_ "Name"
                th_ "Okta status"
            tbody_ $ do
                for_ employees $ \e -> tr_ $ do
                    td_ $ toHtml (e ^. P.employeeFullname)
                    td_ $ for_ (e ^. P.employeeEmail >>= \email -> oktaMap ^. at email) $ \u -> do
                        toHtml $ show $ O.userStatus u
  where
--      numberOfEmployees = length employees
--      numberOfOktaUsers = length users
      oktaMap = Map.fromList $ (\u -> (O.profileLogin $ O.userProfile u, u)) <$> users
      personioEmailMap = Map.fromList $ catMaybes $ fmap (\e -> case e ^. P.employeeEmail of
                                                             Just email -> Just (email, e)
                                                             Nothing -> Nothing) employees
      activeEmployees = filter (\e -> e ^. P.employeeStatus == P.Active) employees
      activeEmployeesNotInOkta = filter (\e -> case e ^. P.employeeEmail >>= \email -> oktaMap ^. at email of
                                            Just _ -> False
                                            Nothing -> True) activeEmployees
      oktaUsersNotInPersonio = filter (\u -> case personioEmailMap ^. at (O.getOktaLogin u) of
                                               Just _ -> False
                                               Nothing -> True) users
