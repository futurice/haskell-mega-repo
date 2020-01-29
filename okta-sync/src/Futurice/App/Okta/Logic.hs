{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Okta.Logic where

import Futurice.Email    (emailToText)
import Futurice.Generics
import Futurice.Office   (officeFromText, officeToText)
import Futurice.Prelude
import Futurice.Tribe    (tribeFromText, tribeToText)
import GitHub            (untagName)
import Prelude ()

import Futurice.App.Okta.Types

import qualified Data.Map  as Map
import qualified Data.Text as T
import qualified Okta      as O
import qualified Personio  as P

groupInfo :: OktaJSON
groupInfo = $(makeRelativeToProject "okta-groups.json" >>= embedFromJSON (Proxy :: Proxy OktaJSON))

groupMap :: Map Text GroupInfo
groupMap = Map.fromList $ (\g -> (giName g, g)) <$> ojGroups groupInfo

internalGroupName :: Text
internalGroupName = ojInternalGroup groupInfo

externalGroupName :: Text
externalGroupName = ojExternalGroup groupInfo

allGroupFuturiceEmployees :: (O.MonadOkta m) => m [O.User]
allGroupFuturiceEmployees = let g = fromMaybe (error "Error while finding group") $ Map.lookup internalGroupName groupMap
                            in fmap (filter (\u -> u ^. O.userProfile . O.profileEmploymentType == Just "external" )) $ O.groupMembers $ giId g

updateUsers :: (O.MonadOkta m) => [P.Employee] -> [O.User] -> m [O.User]
updateUsers employees users = do
    let (_duplicates, singles) = Map.partition (\a -> length a > 1) personioMap
    let singles' = Map.mapMaybe listToMaybe singles
    let peopleToUpdate = catMaybes $ fmap (\(email, ouser) -> Map.lookup email singles' >>= changeData ouser) $ Map.toList loginMap

    -- update user information
    traverse (\c -> O.updateUser (uiOktaId c) (toJSON c)) peopleToUpdate

  where
    loginMap = Map.fromList $ (\u -> (u ^. O.userProfile . O.profileLogin, u)) <$> users

    personioMap = Map.fromListWith (<>) $ catMaybes $ (\e -> e ^. P.employeeEmail >>= \email -> Just (email, [e])) <$> employees

    personioIdMap = Map.fromList $ (\e -> (e ^. P.employeeId, e)) <$> employees

    personioEmployeeToUpdate oktaId pemp =
        UpdateInformation
        { uiOktaId = oktaId
        , uiSecondEmail = maybe "" T.strip (pemp ^. P.employeeHomeEmail)
        , uiEmployeeNumber = Just $ pemp ^. P.employeeId
        , uiTribe = Just $ pemp ^. P.employeeTribe
        , uiOffice = Just $ pemp ^. P.employeeOffice
        , uiEmploymentType = pemp ^. P.employeeEmploymentType
        , uiGender = pemp ^. P.employeeGender
        , uiCountry = pemp ^. P.employeeCountry
        , uiRole = Just $ pemp ^. P.employeeRole
        , uiStartDate = pemp ^. P.employeeHireDate
        , uiManager = pemp ^. P.employeeSupervisorId >>= flip Map.lookup personioIdMap >>= (^. P.employeeEmail)
        , uiFumUsername = pemp ^. P.employeeLogin
        , uiTerminationDate = pemp ^. P.employeeEndDate
        , uiSeparationReason = pemp ^. P.employeeTerminationType
        , uiBirthday = pemp ^. P.employeeBirthday
        }

    oktaUserToUpdate ouser =
        UpdateInformation
        { uiOktaId = ouser ^. O.userId
        , uiSecondEmail = fromMaybe "" $ ouser ^. O.userProfile . O.profileSecondEmail
        , uiEmployeeNumber = P.EmployeeId <$> (readMaybe =<< ouser ^. O.userProfile . O.profileEmployeeNumber)
        , uiTribe = tribeFromText =<< ouser ^. O.userProfile . O.profileTribe
        , uiOffice = officeFromText =<< ouser ^. O.userProfile . O.profileOffice
        , uiEmploymentType = P.employmentTypeFromText =<< ouser ^. O.userProfile . O.profileEmploymentType
        , uiGender = ouser ^. O.userProfile . O.profileGender
        , uiCountry = ouser ^. O.userProfile . O.profileCountry
        , uiRole = ouser ^. O.userProfile . O.profileRole
        , uiStartDate = ouser ^. O.userProfile . O.profileStartDate
        , uiManager = ouser ^. O.userProfile . O.profileManager
        , uiFumUsername = ouser ^. O.userProfile . O.profileFumUsername
        , uiTerminationDate = ouser ^. O.userProfile . O.profileTerminationDate
        , uiSeparationReason = ouser ^. O.userProfile . O.profileSeparationReason
        , uiBirthday = ouser ^. O.userProfile . O.profileBirthday
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
    _notInactiveEmployeesNotInOkta = filter notFoundInOkta $ filter notMachineUser $ filter emailNotEmpty notInactiveEmployees

createUser :: (O.MonadOkta m) => P.Employee -> m O.User
createUser pemp = O.createUser newUser
  where
    listSingleton a = [a]
    newUser = O.NewUser newUserProfile newUserGroupIds
    futuGroup = listSingleton $ giId <$> Map.lookup internalGroupName groupMap
    officeGroup = listSingleton $ giId <$> Map.lookup ("Org-" <> (officeToText $ pemp ^. P.employeeOffice)) groupMap
    tribeGroup = listSingleton $ giId <$> Map.lookup ("Org-" <> (T.replace " " "-" $ tribeToText $ pemp ^. P.employeeTribe)) groupMap
    externalGroup = listSingleton $ giId <$> Map.lookup externalGroupName groupMap
    externalOfficeGroup = listSingleton $ giId <$> Map.lookup (externalGroupName <> "-" <> (officeToText $ pemp ^. P.employeeOffice)) groupMap
    newUserGroupIds | (pemp ^. P.employeeEmploymentType) == Just P.Internal = nub $ catMaybes $ futuGroup <> officeGroup <> tribeGroup
                    | otherwise                                             = nub $ catMaybes $ externalGroup <> externalOfficeGroup <> tribeGroup
    newUserProfile = O.NewUserProfile
        { O.nuFirstName = pemp ^. P.employeeFirst
        , O.nuLastName = pemp ^. P.employeeLast
        , O.nuLogin = fromMaybe "" $ emailToText <$> pemp ^. P.employeeEmail
        , O.nuEmail = fromMaybe "" $ emailToText <$> pemp ^. P.employeeEmail
        , O.nuSecondEmail = fromMaybe "" $ pemp ^. P.employeeHomeEmail
        , O.nuPersonioNumber = pemp ^. P.employeeId
        , O.nuGithubUsername = fromMaybe "" $ untagName <$> pemp ^. P.employeeGithub
        }
