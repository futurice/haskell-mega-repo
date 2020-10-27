{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
-- | https://help.officevibe.com/hc/en-us/articles/115002182731-Custom-Integration
module Futurice.App.Reports.OfficeVibeIntegration where

import Data.Aeson                (Value (Null))
import Data.Char                 (isAlphaNum)
import Data.Maybe                (isJust)
import Data.Tuple                (swap)
import FUM.Types.Login           (Login, loginToText)
import Futurice.Company
import Futurice.Email            (Email)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Office
import Futurice.Prelude
import Futurice.Tribe
import Numeric.Interval.NonEmpty (Interval, (...))
import Prelude ()
import Web.HttpApiData           (toUrlPiece)

import qualified Data.Csv             as CSV
import qualified Data.HashMap.Strict  as HM
import qualified Data.Map.Strict      as Map
import qualified Data.Swagger         as Sw
import qualified Data.Swagger.Declare as Sw
import qualified Data.Text            as T
import qualified Data.Vector          as V
import qualified Personio             as P
import qualified PlanMill             as PM
import qualified PlanMill.Queries     as PMQ

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

-- | IDs can be alphanumerical.
newtype OfficeVibeId tag = OfficeVibeId Text
  deriving stock (Eq, Ord)
  deriving newtype (Show, NFData, ToJSON, FromJSON, CSV.ToField)

-- | Serialised as @TRUE@ and @FALSE@
newtype OfficeVibeBool = OfficeVibeBool Bool
  deriving stock (Eq, Ord)
  deriving newtype (Show, NFData, ToJSON, FromJSON)

data Empty = Empty
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data OfficeVibeUser = OfficeVibeUser
    { ovuId       :: !(OfficeVibeId OfficeVibeUser)
    , ovuEmail    :: !Email
    , ovuFirst    :: !Text
    , ovuLast     :: !Text
    , ovuTitle    :: !Text
    , ovuImageUrl :: !Empty
    }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data OfficeVibeGroup = OfficeVibeGroup
    { ovgId   :: OfficeVibeId OfficeVibeGroup
    , ovgName :: Text
    }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data OfficeVibeRelation = OfficeVibeRelation
    { ovrGroupId    :: !(OfficeVibeId OfficeVibeGroup)
    , ovrUserId     :: !(OfficeVibeId OfficeVibeUser)
    , ovrSubGroupId :: !Empty
    , ovrIsMember   :: !OfficeVibeBool
    , ovrIsManager  :: !OfficeVibeBool
    }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

data OfficeVibeData = OfficeVibeData
    { ovdUsers     :: [OfficeVibeUser]
    , ovdGroups    :: [OfficeVibeGroup]
    , ovdRelations :: [OfficeVibeRelation]
    }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

-------------------------------------------------------------------------------
-- JSON + Swagger
-------------------------------------------------------------------------------

class OfficeVibeIdSchema tag where
    declareIdSchema
        :: proxy (OfficeVibeId tag)
        -> Sw.Declare (Sw.Definitions Sw.Schema) Sw.NamedSchema

instance OfficeVibeIdSchema OfficeVibeUser where
    declareIdSchema _ = pure $ Sw.NamedSchema (Just "ID of user") mempty

instance OfficeVibeIdSchema OfficeVibeGroup where
    declareIdSchema _ = pure $ Sw.NamedSchema (Just "ID of group") mempty

instance OfficeVibeIdSchema tag => ToSchema (OfficeVibeId tag) where
    declareNamedSchema = declareIdSchema


instance ToJSON Empty where
    toJSON _ = Null

instance FromJSON Empty where
    parseJSON _ = return Empty

instance CSV.ToField Empty where
    toField _ = ""

instance ToSchema Empty where
    declareNamedSchema _ = pure $ Sw.NamedSchema (Just "not-user") mempty


instance ToSchema OfficeVibeBool where
    declareNamedSchema _ = Sw.declareNamedSchema (Proxy :: Proxy Bool)

instance CSV.ToField OfficeVibeBool where
    toField (OfficeVibeBool b) = if b then "TRUE" else "FALSE"


deriveGeneric ''OfficeVibeUser
instance ToSchema OfficeVibeUser where declareNamedSchema = sopDeclareNamedSchema
deriveVia [t| ToJSON OfficeVibeUser   `Via` Sopica OfficeVibeUser |]
deriveVia [t| FromJSON OfficeVibeUser `Via` Sopica OfficeVibeUser |]

-- TODO: deriving via RenamedFields '["id", "email", ...]
instance CSV.DefaultOrdered OfficeVibeUser where
    headerOrder _ = V.fromList
        [ "id"
        , "email"
        , "firstName"
        , "lastName"
        , "jobTitle"
        , "imageUrl"
        ]

instance CSV.ToNamedRecord OfficeVibeUser where
    toNamedRecord OfficeVibeUser {..} = HM.fromList
        [ "id"        ~> ovuId
        , "email"     ~> ovuEmail
        , "firstName" ~> ovuFirst
        , "lastName"  ~> ovuLast
        , "jobTitle"  ~> ovuTitle
        , "imageUrl"  ~> Empty
        ]
      where
        k ~> v = (k, CSV.toField v)

deriveGeneric ''OfficeVibeGroup
instance ToSchema OfficeVibeGroup where declareNamedSchema = sopDeclareNamedSchema
deriveVia [t| ToJSON OfficeVibeGroup   `Via` Sopica OfficeVibeGroup |]
deriveVia [t| FromJSON OfficeVibeGroup `Via` Sopica OfficeVibeGroup |]

instance CSV.DefaultOrdered OfficeVibeGroup where
    headerOrder _ = V.fromList
        [ "id"
        , "name"
        ]

instance CSV.ToNamedRecord OfficeVibeGroup where
    toNamedRecord OfficeVibeGroup {..} = HM.fromList
        [ "id"   ~> ovgId
        , "name" ~> ovgName
        ]
      where
        k ~> v = (k, CSV.toField v)

deriveGeneric ''OfficeVibeRelation
instance ToSchema OfficeVibeRelation where declareNamedSchema = sopDeclareNamedSchema
deriveVia [t| ToJSON OfficeVibeRelation   `Via` Sopica OfficeVibeRelation |]
deriveVia [t| FromJSON OfficeVibeRelation `Via` Sopica OfficeVibeRelation |]

instance CSV.DefaultOrdered OfficeVibeRelation where
    headerOrder _ = V.fromList
        [ "groupId"
        , "userId"
        , "subGroupId"
        , "isMember"
        , "isManager"
        ]

instance CSV.ToNamedRecord OfficeVibeRelation where
    toNamedRecord OfficeVibeRelation {..} = HM.fromList
        [ "groupId"    ~> ovrGroupId
        , "userId"     ~> ovrUserId
        , "subGroupId" ~> ovrSubGroupId
        , "isMember"   ~> ovrIsMember
        , "isManager"  ~> ovrIsManager
        ]
      where
        k ~> v = (k, CSV.toField v)

-------------------------------------------------------------------------------
-- Fetch Data
-------------------------------------------------------------------------------

officeVibeData
    :: forall m. (MonadTime m, MonadPersonio m, MonadPlanMillQuery m)
    => m OfficeVibeData
officeVibeData = do
    today <- currentDay
    let interval = beginningOfPrev2Month today ... pred today

    fpm0 <- personioPlanmillMap
    let fpm = HM.filter (employeePredicate today . fst) fpm0 -- TODO: type signature

    let uidLookup = Map.fromList
          [ (pmu ^. PM.identifier, p)
          | (p, pmu) <- toList fpm
          ]

    -- TODO filter My Company accounts?
    activeAccounts <- mangleActiveAccounts today uidLookup <$>
        (traverse . traverse) (perEmployee interval) fpm

    let hardRelations = fpm ^.. folded . _1 . getter hardGroupRelations . folded

    return OfficeVibeData
        { ovdUsers     = fpm ^.. folded . _1 . getter toUser . _Just
        , ovdGroups    = hardGroups ++ activeAccountsGroups activeAccounts
        , ovdRelations = hardRelations ++ activeAccountsRelations activeAccounts
        }
  where
    employeePredicate :: Day -> P.Employee -> Bool
    employeePredicate today p =
        p ^. P.employeeEmploymentType == Just P.Internal
        && P.employeeIsActive today p
        && isJust (p ^. P.employeeEmail)

    toUser :: P.Employee -> Maybe OfficeVibeUser
    toUser p = do
        email <- p ^. P.employeeEmail
        return OfficeVibeUser
            { ovuId       = employeeToId p
            , ovuEmail    = email
            , ovuFirst    = p ^. P.employeeFirst
            , ovuLast     = p ^. P.employeeLast
            , ovuTitle    = p ^. P.employeeRole
            , ovuImageUrl = Empty
            }

-------------------------------------------------------------------------------
-- Hard coded groups
-------------------------------------------------------------------------------

    hardGroups :: [OfficeVibeGroup]
    hardGroups = tribes ++ offices ++ countries
      where
        tribes =
            [ OfficeVibeGroup (tribeToId t) (tribeToText t)
            | t <- [ minBound .. maxBound ]
            ]

        countries =
            [ OfficeVibeGroup (countryToId t) (countryToText t)
            | t <- [ minBound .. maxBound ]
            ]

        offices =
            [ OfficeVibeGroup (officeToId t) (officeToText t)
            | t <- [ minBound .. maxBound ]
            ]

    hardGroupRelations :: P.Employee -> [OfficeVibeRelation]
    hardGroupRelations p =
        [ OfficeVibeRelation (tribeToId (p ^. P.employeeTribe))     (employeeToId p) Empty true false
        , OfficeVibeRelation (officeToId (p ^. P.employeeOffice))   (employeeToId p) Empty true false
        , OfficeVibeRelation (countryToId' (p ^. P.employeeCountry)) (employeeToId p) Empty true false
        ]

    employeeToId :: P.Employee -> OfficeVibeId OfficeVibeUser
    employeeToId p = OfficeVibeId $
        (p ^. P.employeeId . getter toUrlPiece)
        <> maybe "" (\login -> "-" <> loginToText login) (p ^. P.employeeLogin)

    tribeToId :: Tribe -> OfficeVibeId OfficeVibeGroup
    tribeToId t = toGroupId "tribe" (tribeToText t)

    officeToId :: Office -> OfficeVibeId OfficeVibeGroup
    officeToId t = toGroupId "office" (officeToText t)

    countryToId :: Country -> OfficeVibeId OfficeVibeGroup
    countryToId t = toGroupId "country" (countryToText' t)

    countryToId' :: Maybe Country -> OfficeVibeId OfficeVibeGroup
    countryToId' (Just t) = toGroupId "country" (countryToText' t)
    countryToId' Nothing  = toGroupId "country" "no-country"

-------------------------------------------------------------------------------
-- Misc
-------------------------------------------------------------------------------

    true = OfficeVibeBool True
    false = OfficeVibeBool False

    toGroupId :: Text -> Text -> OfficeVibeId OfficeVibeGroup
    toGroupId pfx t = OfficeVibeId $ pfx <> "-" <> T.map escape (T.toLower t) where
        escape c
            | isAlphaNum c = c
            | otherwise = '-'

-------------------------------------------------------------------------------
-- Active accounts
-------------------------------------------------------------------------------

    activeAccountsGroups :: Map k ActiveAccount -> [OfficeVibeGroup]
    activeAccountsGroups xs = xs ^.. folded . getter impl where
        impl (ActiveAccount n _ _) = OfficeVibeGroup (accountNameId n) n

    accountNameId :: Text -> OfficeVibeId OfficeVibeGroup
    accountNameId = toGroupId "account"

    activeAccountsRelations :: Map k ActiveAccount -> [OfficeVibeRelation]
    activeAccountsRelations xs = xs ^.. folded . getter impl . folded where
        impl (ActiveAccount n owners ps) =
            -- active employees in account
            [ OfficeVibeRelation (accountNameId n) (employeeToId p) Empty true isManager
            | (pid, p) <- Map.toList ps
            , let isManager = OfficeVibeBool $ pid `elem` ownersIds
            ] ++
            -- owners which aren't active in the account
            [ OfficeVibeRelation (accountNameId n) (employeeToId o) Empty false true
            | (oid, o) <- Map.toList owners
            , oid `notElem` pIds
            ]
          where
            ownersIds = Map.keysSet owners
            pIds      = Map.keysSet ps

    perEmployee :: Interval Day -> PM.User -> m (PM.User, Map PM.AccountId (Day, PM.Account))
    perEmployee interval pmu = do
        let uid = pmu ^. PM.identifier
        trs <- PMQ.timereports interval uid
        let projIds = map swap $ Map.toList $ Map.fromListWith max
              [ (i, PM.trStart tr)
              | tr <- toList trs
              , Just i <- [PM.trProject tr]
              ]
        projects <- (traverse . traverse) PMQ.simpleProject projIds
        let accIds = Map.fromListWith max
              [ (i, d)
              | (d, pr) <- projects
              , Just i  <- [pr ^. PM.pAccount]
              ]
        accounts <- ifor accIds $ \i d -> (,) d <$> PMQ.account i
        return (pmu, accounts)

    mangleActiveAccounts
        :: Day
        -> Map PM.UserId P.Employee
        -> HashMap Login (P.Employee, (PM.User, Map PM.AccountId (Day, PM.Account)))
        -> Map (OfficeVibeId OfficeVibeGroup) ActiveAccount
    mangleActiveAccounts _today uidLookup xs = Map.fromListWith app
        [ pair (accountNameId $ PM.saName acc) $ ActiveAccount
            { aaName      = PM.saName acc
            , aaOwners    = maybe Map.empty id $ do
                oid   <- PM.saOwner acc
                owner <- uidLookup ^? ix oid
                return $ Map.singleton (owner ^. P.employeeId) owner
            , aaEmployees = Map.singleton (pe ^. P.employeeId) pe
            }
        | (_login, (pe, (_pmu, ys))) <- HM.toList xs
        , (_accId, (_d, acc)) <- Map.toList ys
        ]
      where
        app (ActiveAccount a o b) (ActiveAccount _ o'  b') =
            ActiveAccount a (o <> o') (b <> b')

        pair = (,)

data ActiveAccount = ActiveAccount
    { aaName      :: !Text
    , aaOwners    :: !(Map P.EmployeeId P.Employee)
    , aaEmployees :: !(Map P.EmployeeId P.Employee)
    }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)
