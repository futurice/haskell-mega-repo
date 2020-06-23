{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Futurice.App.Okta.Logic where

import Futurice.App.Okta.Config
import Futurice.App.Okta.Ctx
import Futurice.CareerLevel     (careerLevelToText)
import Futurice.Email           (emailFromText, emailToText)
import Futurice.Generics
import Futurice.Office          (officeFromText, officeToText)
import Futurice.Prelude
import Futurice.Tribe           (tribeFromText, tribeToText)
import GitHub                   (untagName)
import Prelude ()

import Futurice.App.Okta.Types

import qualified Data.Map  as Map
import qualified Data.Set  as S
import qualified Data.Text as T
import qualified Okta      as O
import qualified Personio  as P
import qualified Power

groupInfo :: OktaJSON
groupInfo = $(makeRelativeToProject "okta-groups.json" >>= embedFromJSON (Proxy :: Proxy OktaJSON))

groupMap :: Map Text GroupInfo
groupMap = Map.fromList $ (\g -> (giName g, g)) <$> ojGroups groupInfo

internalGroupName :: Text
internalGroupName = ojInternalGroup groupInfo

internalGroup :: Text
internalGroup = maybe (error "No internal group found") giId $ Map.lookup internalGroupName groupMap

externalGroupName :: Text
externalGroupName = ojExternalGroup groupInfo

peakonGroup :: Text
peakonGroup = maybe (error "No App-Peakon group found") giId $ Map.lookup "App-Peakon" groupMap

allGroupFuturiceEmployees :: (O.MonadOkta m) => m [O.User]
allGroupFuturiceEmployees = let g = fromMaybe (error "Error while finding group") $ Map.lookup internalGroupName groupMap
                            in fmap (filter (\u -> u ^. O.userProfile . O.profileEmploymentType == Just "external" )) $ O.groupMembers $ giId g

data OktaUpdateStats = OktaUpdateStats
    { updatedUsers :: ![O.User]
    , removedUsers :: !Int
    , addedUsers   :: !Int
    }

data ClientStatus = MainClient Text
                  | NoClient
                  | Other
                  deriving (Ord, Eq, Show)

fetchClientInformation :: (Power.MonadPower m) => Day -> [P.Employee] -> m (Map.Map P.EmployeeId ClientStatus)
fetchClientInformation now emps = do
    persons <- Power.powerPeople
    let personsMap = Map.fromList $ (\p -> (Power.personLogin p, p)) <$> persons

    customers <- filter (\c -> Power.customerInternalCustomer c == False) <$> Power.powerCustomers
    projects <- Power.powerProjects
    allocations <- Power.powerAllocations

    let employeeAllocations emp = filter (\a -> Power.allocationPersonId a == Just (Power.personId emp)) allocations
    let filterCurrentAllocations = filter (\a -> utctDay (Power.allocationStartDate a) <= now && now <= utctDay (Power.allocationEndDate a) && Power.allocationProposed a == False)
    let filterMainAllocations = filter (\a -> Power.allocationTotalAllocation a > 0.5)
    let getRelevantAllocations as = filterMainAllocations $ filterCurrentAllocations as

    let isOnProject as p = Power.projectId p `elem` (Power.allocationProjectId <$> getRelevantAllocations as)
    let employeeProjects as = filter (isOnProject as) projects
    let employeeCustomers ps = filter (\c -> elem (Power.customerId c) (Power.projectCustomerId <$> ps)) customers

    let employeeMainProject emp =
            let employeeCustomer e = listToMaybe $ Power.customerName <$> (employeeCustomers $ employeeProjects $ employeeAllocations e)
                clientStatus =
                    case emp ^. P.employeeLogin >>= \x -> personsMap ^.at x of
                      Just e | Power.personUtzTarget e > 0 ->
                                   let allocs = filterCurrentAllocations $ employeeAllocations e
                                   in if length allocs > 0 then
                                        maybe Other MainClient $ employeeCustomer e
                                      else
                                        NoClient
                      _ -> Other
            in (emp ^. P.employeeId, clientStatus)
    pure $ Map.fromList $ employeeMainProject <$> emps

updateUsers :: (O.MonadOkta m, Power.MonadPower m) => Ctx -> Day -> [P.Employee] -> [O.User] -> m OktaUpdateStats
updateUsers ctx now employees users = do
    let (_duplicates, singles) = Map.partition (\a -> length a > 1) personioMap
    let singles' = Map.mapMaybe listToMaybe singles

    clientInformation <- fetchClientInformation now employees

    let peopleToUpdate = catMaybes $ fmap (\(email, ouser) -> emailFromText email >>= \email' -> Map.lookup email' singles' >>= changeData clientInformation ouser) $ Map.toList loginMap

    -- update user information
    updated <- traverse (\c -> O.updateUser (uiOktaId c) (toJSON c)) peopleToUpdate

    -- remove people from group
    members <- O.groupMembers peakonGroup
    let members' = S.fromList $ map (^. O.userId) $ filter (\m -> maybe True (`S.notMember` employeeExceptions) $ (m ^. O.userProfile . O.profileEmployeeNumber >>= readMaybe >>= Just . P.EmployeeId)) members
    let membersNotActiveAnymore = filter (\u -> not $ u `S.member` (S.fromList activeInternalEmployees)) $ S.toList members'

    removed <- traverse (\u -> O.deleteUserFromGroup peakonGroup u) $ membersNotActiveAnymore

    -- add people to group
    let employeeNotInGroup = filter (\u -> not $ u `S.member` members') activeInternalEmployees

    added <- traverse (\u -> O.addUserToGroup peakonGroup u) $ employeeNotInGroup

    pure $ OktaUpdateStats updated (length removed) (length added)
  where
    -- follow all clients with at least 4 internal employees
    customersToFollow cmap = Map.keys
        $ Map.filter (\emps -> length emps > 3)
        $ Map.map (filter (\eid ->
              case personioIdMap ^.at eid of
                Just emp | emp ^. P.employeeEmploymentType == Just P.Internal -> True
                _ -> False))
        $ Map.fromListWith (<>) $ map (\(a,b) -> (b,[a])) $ Map.toList cmap

    activeInternalEmployees = catMaybes $ map toOktaId $ filter (\e -> e ^. P.employeeEmploymentType == Just P.Internal) $ filter (\e -> e ^. P.employeeStatus == P.Active) employees

    toOktaId emp =
        case emp ^. P.employeeEmail >>= \email -> Map.lookup (emailToText email) loginMap of
            Just ouser -> Just $ ouser ^. O.userId
            Nothing -> Nothing

    loginMap = Map.fromList $ (\u -> (u ^. O.userProfile . O.profileLogin, u)) <$> users

    personioMap = Map.fromListWith (<>) $ catMaybes $ (\e -> e ^. P.employeeEmail >>= \email -> Just (email, [e])) <$> employees

    personioIdMap = Map.fromList $ (\e -> (e ^. P.employeeId, e)) <$> employees

    personioEmployeeToUpdate clientInformation oktaId pemp =
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
        , uiCompetenceHome = pemp ^. P.employeeCompetenceHome
        , uiMatrixSupervisor = pemp ^. P.employeeMatrixSupervisorEmail
        , uiMobilePhone = pemp ^. P.employeeWorkPhone
        , uiClientAccount = clientInformation ^.at (pemp ^. P.employeeId)
          <&> \info ->
                case info of
                    MainClient client | info `elem` customersToFollow clientInformation -> client
                    NoClient -> "No client"
                    _ -> "Other"
        , uiCareerLevel = let levelToText (n : _) = readMaybe [n]
                              levelToText _ = Nothing
                          in pemp ^. P.employeeCareerLevel >>= levelToText . T.unpack . careerLevelToText
        , uiDisplayName = Just $ pemp ^. P.employeeFullname
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
        , uiCompetenceHome = ouser ^. O.userProfile . O.profileCompetenceHome
        , uiMatrixSupervisor = ouser ^. O.userProfile . O.profileMatrixSupervisor
        , uiMobilePhone = ouser ^. O.userProfile . O.profileMobilePhone
        , uiClientAccount = ouser ^. O.userProfile . O.profileClientAccount
        , uiCareerLevel = ouser ^. O.userProfile . O.profileCareerLevel
        , uiDisplayName = ouser ^. O.userProfile . O.profileDisplayName
        }

    changeData clientInformation ouser pemp =
        let currentInfo = personioEmployeeToUpdate clientInformation (ouser ^. O.userId) pemp
            oldInformation = oktaUserToUpdate ouser
        in if oldInformation /= currentInfo then
             Just currentInfo
           else
             Nothing

    machineUsers = P.EmployeeId <$> [1436090, 982480]
    notMachineUser e = not $ e ^. P.employeeId `elem` machineUsers
    notInactiveEmployees = filter (\e -> e ^. P.employeeStatus /= P.Inactive) employees
    notFoundInOkta e =
        case e ^. P.employeeEmail >>= \email -> loginMap ^. at (emailToText email) of
          Just _ -> False
          Nothing -> True
    emailNotEmpty e = e ^. P.employeeEmail /= Nothing && (e ^. P.employeeEmail >>= Just . emailToText) /= Just ""
    _notInactiveEmployeesNotInOkta = filter notFoundInOkta $ filter notMachineUser $ filter emailNotEmpty notInactiveEmployees

    employeeExceptions = S.fromList $ cfgAlwaysInPeakon $ ctxConfig ctx

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
