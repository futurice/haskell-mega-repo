{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Okta.IndexPage where

import Futurice.Email            (emailFromText, emailToText)
import Futurice.Lucid.Foundation hiding (page_)
import Futurice.Prelude
import Prelude ()

import Futurice.App.Okta.Markup

import qualified Data.Map  as Map
import qualified Data.Text as T
import qualified Okta      as O
import qualified Peakon    as PK
import qualified Personio  as P

indexPage :: [P.Employee] -> [O.User] -> [O.User] -> [PK.Employee] -> HtmlPage "indexpage"
indexPage employees users internalGroupUsers peakonEmployees = page_ "Okta sync" (Just NavHome) $ do
    fullRow_ $ do
        button_ [ id_ "start-okta-sync", class_ "button" ] "Sync with Personio"
    fullRow_ $ do
        h2_ "Not inactive people in Personio that are not in Okta"
        sortableTable_ $ do
            thead_ $ do
                th_ mempty
                th_ "#"
                th_ "Name"
                th_ "Start"
                th_ "Employment"
            tbody_ $ do
                for_ (sortOn P._employeeHireDate notInactiveEmployeesNotInOkta) $ \e -> tr_ $ do
                    td_ $ checkbox_ False [ data_ "okta-add-user" $ employeeNumber $ e ^. P.employeeId]
                    td_ $ toHtml (e ^. P.employeeId)
                    td_ $ toHtml (e ^. P.employeeFullname)
                    td_ $ toHtml $ maybe "" show (e ^. P.employeeHireDate)
                    td_ $ toHtml $ maybe "" show (e ^. P.employeeEmploymentType)
        div_ [ class_ "button-group" ] $
            button_ [ id_ "add-users", class_ "button alert", disabled_ "disabled" ] "Add to Okta"
        h2_ "People in Okta that are inactive in Personio"
        sortableTable_ $ do
            thead_ $ do
                th_ "#"
                th_ "Okta"
                th_ "Name"
                th_ "Contract end date"
            tbody_ $ do
                for_ (sortOn (P._employeeEndDate . snd) inactiveInPersonio) $ \(u,e) -> tr_ $ do
                    td_ $ toHtml (e ^. P.employeeId)
                    td_ $ toHtml $ u ^. O.userId
                    td_ $ toHtml (e ^. P.employeeFullname)
                    td_ $ toHtml $ maybe "" textShow (e ^. P.employeeEndDate)
        h2_ "People in Okta that are not in Personio"
        sortableTable_ $ do
            thead_ $ do
                th_ "Name"
                th_ "Login"
            tbody_ $ do
                for_ oktaUsersNotInPersonio $ \u -> tr_ $ do
                    td_ $ toHtml $ (u ^. O.userProfile . O.profileFirstName) <> " " <> (u ^. O.userProfile . O.profileLastName)
                    td_ $ toHtml $ u ^. O.userProfile . O.profileLogin
        h2_ "People with incorrect termination date in Peakon"
        sortableTable_ $ do
            thead_ $ do
                th_ "Name"
                th_ "Personio contract end date"
                th_ "Peakon termination date"
            tbody_ $ do
                for_ incorrectTerminationDate $ \(d,pemp) -> tr_ $ do
                    td_ $ toHtml $ pemp ^. P.employeeFullname
                    td_ $ toHtml $ maybe "" textShow $ pemp ^. P.employeeEndDate
                    td_ $ toHtml $ textShow d
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
                    td_ $ for_ (e ^. P.employeeEmail >>= \email -> loginMap ^. at (emailToText email)) $ \u -> do
                        toHtml $ show $ u ^. O.userStatus
  where
    employeeNumber (P.EmployeeId n) = textShow n
    loginMap = Map.fromList $ (\u -> (u ^. O.userProfile . O.profileLogin, u)) <$> users
    personioMap = Map.fromListWith (<>) $ catMaybes $ (\e -> e ^. P.employeeEmail >>= \email -> Just (email, [e])) <$> employees

    machineUsers = P.EmployeeId <$> [1436090, 982480]
    notMachineUser e = not $ e ^. P.employeeId `elem` machineUsers
    notInactiveEmployees = filter (\e -> e ^. P.employeeStatus /= P.Inactive) employees

    oktaUser :: P.Employee -> Maybe O.User
    oktaUser e = e ^. P.employeeEmail >>= \email -> loginMap ^. at (emailToText email)

    notFoundInOkta e =
        case e ^. P.employeeEmail >>= \email -> loginMap ^. at (emailToText email) of
          Just _ -> False
          Nothing -> True
    emailNotEmpty e = e ^. P.employeeEmail /= Nothing && (e ^. P.employeeEmail >>= Just . emailToText) /= Just ""
    notInactiveEmployeesNotInOkta = filter notFoundInOkta $ filter notMachineUser $ filter emailNotEmpty notInactiveEmployees

    oktaUsersNotInPersonio = filter (\u -> case (emailFromText $ u ^. O.userProfile . O.profileLogin) >>= \email -> personioMap ^. at email of
                                             Just _ -> False
                                             Nothing -> True) users

    oktaFuturiceMember = filter (\u -> case emailFromText (u ^. O.userProfile . O.profileLogin) >>= \email -> personioMap ^. at email of
                                    Just (e:_) | u `elem` internalGroupUsers -> not (e ^. P.employeeStatus == P.Active && e ^. P.employeeEmploymentType == Just P.Internal)
                                               | otherwise -> False
                                    _ -> True) users

    inactiveInPersonio :: [(O.User, P.Employee)]
    inactiveInPersonio = catMaybes $ fmap (\e -> (,) <$> oktaUser e <*> Just e) $ filter (\e -> e ^. P.employeeStatus == P.Inactive) employees

    peakonMap :: Map P.EmployeeId PK.Employee
    peakonMap = Map.fromList $ catMaybes $ map (\pk -> (,) <$> (Just . P.EmployeeId =<< readMaybe . T.unpack =<< PK._employeeIdentifier pk) <*> Just pk) peakonEmployees

    incorrectTerminationDate :: [(Maybe Day, P.Employee)]
    incorrectTerminationDate =
        let checkTerminationDate p = case peakonMap ^.at (p ^. P.employeeId) >>= PK._employeeTerminationDate of
                                       d | d /= (p ^. P.employeeEndDate) -> Just (d, p)
                                       _ -> Nothing
        in catMaybes $ map checkTerminationDate $ filter (\e -> e ^. P.employeeEmploymentType == Just P.Internal && e ^. P.employeeStatus == P.Active) $ employees
