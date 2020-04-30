{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Okta.IndexPage where

import Futurice.Email            (emailToText)
import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Okta.Markup

import qualified Data.Map as Map
import qualified Okta     as O
import qualified Personio as P

indexPage :: [P.Employee] -> [O.User] -> [O.User] -> HtmlPage "indexpage"
indexPage employees users internalGroupUsers = page_ "Okta sync" (Just NavHome) $ do
    fullRow_ $ do
        h2_ "Not inactive people in Personio that are not in Okta"
        sortableTable_ $ do
            thead_ $ do
                th_ mempty
                th_ "#"
                th_ "Name"
                th_ "Employment"
            tbody_ $ do
                for_ notInactiveEmployeesNotInOkta $ \e -> tr_ $ do
                    td_ $ checkbox_ False [ data_ "okta-add-user" $ employeeNumber $ e ^. P.employeeId]
                    td_ $ toHtml (e ^. P.employeeId)
                    td_ $ toHtml (e ^. P.employeeFullname)
                    td_ $ toHtml $ maybe "" show (e ^. P.employeeEmploymentType)
        div_ [ class_ "button-group" ] $
            button_ [ id_ "add-users", class_ "button alert", disabled_ "disabled" ] "Add to Okta"
        h2_ "People in Okta that are inactive in Personio"
        sortableTable_ $ do
            thead_ $ do
                th_ "#"
                th_ "Name"
            tbody_ $ do
                for_ inactiveInPersonio $ \e -> tr_ $ do
                    td_ $ toHtml (e ^. P.employeeId)
                    td_ $ toHtml (e ^. P.employeeFullname)
        h2_ "People in Okta that are not in Personio"
        sortableTable_ $ do
            thead_ $ do
                th_ "Name"
                th_ "Login"
            tbody_ $ do
                for_ oktaUsersNotInPersonio $ \u -> tr_ $ do
                    td_ $ toHtml $ (u ^. O.userProfile . O.profileFirstName) <> " " <> (u ^. O.userProfile . O.profileLastName)
                    td_ $ toHtml $ u ^. O.userProfile . O.profileLogin
        h2_ "People in Futurice group that are not active internal employees in Personio"
        sortableTable_ $ do
            thead_ $ do
                th_ "Name"
            tbody_ $ do
                for_ oktaFuturiceMember $ \u -> tr_ $ do
                    td_ $ toHtml $ u ^. O.userProfile . O.profileFirstName <> " " <> u ^. O.userProfile . O.profileLastName
        h2_ "All employees in Personio"
        sortableTable_ $ do
            thead_ $ do
                th_ "Name"
                th_ "Employment"
                th_ "Okta status"
            tbody_ $ do
                for_ employees $ \e -> tr_ $ do
                    td_ $ toHtml (e ^. P.employeeFullname)
                    td_ $ toHtml $ maybe "" show (e ^. P.employeeEmploymentType)
                    td_ $ for_ (e ^. P.employeeEmail >>= \email -> loginMap ^. at email) $ \u -> do
                        toHtml $ show $ u ^. O.userStatus
  where
    employeeNumber (P.EmployeeId n) = textShow n
    loginMap = Map.fromList $ (\u -> (u ^. O.userProfile . O.profileLogin, u)) <$> users
    personioMap = Map.fromListWith (<>) $ catMaybes $ (\e -> e ^. P.employeeEmail >>= \email -> Just (email, [e])) <$> employees

    machineUsers = P.EmployeeId <$> [1436090, 982480]
    notMachineUser e = not $ e ^. P.employeeId `elem` machineUsers
    notInactiveEmployees = filter (\e -> e ^. P.employeeStatus /= P.Inactive) employees
    notFoundInOkta e =
        case e ^. P.employeeEmail >>= \email -> loginMap ^. at email of
          Just _ -> False
          Nothing -> True
    emailNotEmpty e = e ^. P.employeeEmail /= Nothing && (e ^. P.employeeEmail >>= Just . emailToText) /= Just ""
    notInactiveEmployeesNotInOkta = filter notFoundInOkta $ filter notMachineUser $ filter emailNotEmpty notInactiveEmployees

    oktaUsersNotInPersonio = filter (\u -> case personioMap ^. at (u ^. O.userProfile . O.profileLogin) of
                                             Just _ -> False
                                             Nothing -> True) users

    oktaFuturiceMember = filter (\u -> case personioMap ^. at (u ^. O.userProfile . O.profileLogin) of
                                    Just (e:_) | u `elem` internalGroupUsers -> not (e ^. P.employeeStatus == P.Active && e ^. P.employeeEmploymentType == Just P.Internal)
                                               | otherwise -> False
                                    _ -> True) users
    inactiveInPersonio = filter (not . notFoundInOkta) $ filter (\e -> e ^. P.employeeStatus == P.Inactive) employees
