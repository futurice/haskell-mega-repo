{-# LANGUAGE OverloadedStrings #-}
module Futurice.App.Okta.Logic where

import Data.Aeson        (object, (.=))
import Futurice.Email    (emailToText)
import Futurice.Generics
import Futurice.Office   (Office, officeFromText, officeToText)
import Futurice.Prelude
import Futurice.Tribe    (Tribe, tribeFromText, tribeToText)
import GitHub            (untagName)
import Prelude ()

import qualified Data.Map  as Map
import qualified Data.Text as T
import qualified Okta      as O
import qualified Personio  as P

data UpdateInformation = UpdateInformation
    { uiOktaId         :: !O.OktaId
    , uiSecondEmail    :: !Text
    , uiEmployeeNumber :: !(Maybe P.EmployeeId)
    , uiTribe          :: !(Maybe Tribe)
    , uiOffice         :: !(Maybe Office)
    , uiEmploymentType :: !(Maybe P.EmploymentType)
    } deriving (Eq, Show)

instance ToJSON UpdateInformation where
    toJSON i = object $
        [ "secondEmail"    .= uiSecondEmail i
        , "tribe"          .= (tribeToText <$> uiTribe i)
        , "office"         .= (officeToText <$> uiOffice i)
        , "employeeNumber" .= uiEmployeeNumber i
        , "employmentType" .= (P.employmentTypeToText <$> uiEmploymentType i)
        ]

updateUsers :: (O.MonadOkta m) => [P.Employee] -> [O.User] -> m [O.User]
updateUsers employees users = do
    let (_duplicates, singles) = Map.partition (\a -> length a > 1) personioMap
    let singles' = Map.mapMaybe listToMaybe singles
    let peopleToUpdate = catMaybes $ fmap (\(email, ouser) -> Map.lookup email singles' >>= changeData ouser) $ Map.toList loginMap

    -- update user information
    traverse (\c -> O.updateUser (uiOktaId c) (toJSON c)) peopleToUpdate

    -- create new users
    -- traceShow notInactiveEmployeesNotInOkta $ pure ()
    --traceShow peopleToUpdate $ pure []
  where
    empToString (P.EmployeeId eid) = show eid

    loginMap = Map.fromList $ (\u -> (u ^. O.userProfile . O.profileLogin, u)) <$> users

    personioMap = Map.fromListWith (<>) $ catMaybes $ (\e -> e ^. P.employeeEmail >>= \email -> Just (email, [e])) <$> employees

    personioEmployeeToUpdate oktaId pemp =
        UpdateInformation
        { uiOktaId = oktaId
        , uiSecondEmail = maybe "" T.strip (pemp ^. P.employeeHomeEmail)
        , uiEmployeeNumber = Just $ pemp ^. P.employeeId
        , uiTribe = Just $ pemp ^. P.employeeTribe
        , uiOffice = Just $ pemp ^. P.employeeOffice
        , uiEmploymentType = pemp ^. P.employeeEmploymentType
        }

    oktaUserToUpdate ouser =
        UpdateInformation
        { uiOktaId = ouser ^. O.userId
        , uiSecondEmail = fromMaybe "" $ ouser ^. O.userProfile . O.profileSecondEmail
        , uiEmployeeNumber = P.EmployeeId <$> (readMaybe =<< ouser ^. O.userProfile . O.profileEmployeeNumber)
        , uiTribe = tribeFromText =<< ouser ^. O.userProfile . O.profileTribe
        , uiOffice = officeFromText =<< ouser ^. O.userProfile . O.profileOffice
        , uiEmploymentType = P.employmentTypeFromText =<< ouser ^. O.userProfile . O.profileEmploymentType
        }

    changeData ouser pemp =
        let currentInfo = personioEmployeeToUpdate (ouser ^. O.userId) pemp
            oldInformation = oktaUserToUpdate ouser
        in if oldInformation /= currentInfo then
             Just currentInfo
           else
             Nothing

    machineUsers = P.EmployeeId <$> [1436090, 982480]
    notMachineUser e = not $ e ^. P.employeeId `elem` machineUsers
    notInactiveEmployees = filter (\e -> e ^. P.employeeStatus /= P.Inactive) employees
    notFoundInOkta e =
        case e ^. P.employeeEmail >>= \email -> loginMap ^. at email of
          Just _ -> False
          Nothing -> True
    emailNotEmpty e = e ^. P.employeeEmail /= Nothing && (e ^. P.employeeEmail >>= Just . emailToText) /= Just ""
    notInactiveEmployeesNotInOkta = filter notFoundInOkta $ filter notMachineUser $ filter emailNotEmpty notInactiveEmployees

createUser :: (O.MonadOkta m) => P.Employee -> m O.User
createUser pemp = O.createUser newUser
  where
    newUser = O.NewUser
        { O.nuFirstName = pemp ^. P.employeeFirst
        , O.nuLastName = pemp ^. P.employeeLast
        , O.nuLogin = fromMaybe "" $ emailToText <$> pemp ^. P.employeeEmail
        , O.nuEmail = fromMaybe "" $ emailToText <$> pemp ^. P.employeeEmail
        , O.nuSecondEmail = fromMaybe "" $ pemp ^. P.employeeHomeEmail
        , O.nuPersonioNumber = pemp ^. P.employeeId
        , O.nuGithubUsername = fromMaybe "" $ untagName <$> pemp ^. P.employeeGithub
        }
