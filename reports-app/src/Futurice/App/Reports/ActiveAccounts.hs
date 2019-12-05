{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Futurice.App.Reports.ActiveAccounts (
    -- * Report
    activeAccountsData,
    -- * Types
    ActiveAccounts (..),
    ActiveAccount (..),
    ActiveEmployee (..),
    ) where

import Data.Fixed                (Centi)
import Data.Tuple                (swap)
import Futurice.Email            (Email)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Futurice.Time             (NDT, TimeUnit (..), ndtConvert')
import Numeric.Interval.NonEmpty (Interval, inf, sup, (...))
import Prelude ()

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as Map
import qualified FUM.Types.Login     as FUM
import qualified Personio            as P
import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ

data ActiveEmployee = ActiveEmployee
    { aeName           :: !Text
    , aeDay            :: !Day
    , aeHours          :: !(NDT 'Hours Centi)
    , aeEmail          :: !(Maybe Email)
    , aePlanmillId     :: !PM.UserId
    , aeEmploymentType :: !(Maybe P.EmploymentType)
    }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

mkActiveEmployee :: Day -> NDT 'Hours Centi -> P.Employee -> PM.User -> ActiveEmployee
mkActiveEmployee d h pe pmu = ActiveEmployee
    { aeName           = pe ^. P.employeeFullname
    , aeDay            = d
    , aeHours          = h
    , aeEmail          = pe ^. P.employeeEmail
    , aePlanmillId     = pmu ^. PM.identifier
    , aeEmploymentType = pe ^. P.employeeEmploymentType
    }

deriveGeneric ''ActiveEmployee
instance ToSchema ActiveEmployee where declareNamedSchema = sopDeclareNamedSchema
deriveVia [t| ToJSON ActiveEmployee   `Via` Sopica ActiveEmployee |]
deriveVia [t| FromJSON ActiveEmployee `Via` Sopica ActiveEmployee |]

data ActiveAccount = ActiveAccount
    { aaName      :: !Text
    , aaOwner     :: !(Maybe Email)
    , aaEmployees :: !(Map FUM.Login ActiveEmployee)
    }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

deriveGeneric ''ActiveAccount
instance ToSchema ActiveAccount where declareNamedSchema = sopDeclareNamedSchema
deriveVia [t| ToJSON ActiveAccount   `Via` Sopica ActiveAccount |]
deriveVia [t| FromJSON ActiveAccount `Via` Sopica ActiveAccount |]

data ActiveAccounts = ActiveAccounts
    { aaInterval :: Interval Day
    , aaData     :: Map PM.AccountId ActiveAccount
    }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

deriveGeneric ''ActiveAccounts
instance ToSchema ActiveAccounts where declareNamedSchema = sopDeclareNamedSchema
deriveVia [t| ToJSON ActiveAccounts   `Via` Sopica ActiveAccounts |]
deriveVia [t| FromJSON ActiveAccounts `Via` Sopica ActiveAccounts |]

activeAccountsData
    :: forall m. (MonadTime m, MonadPersonio m, MonadPlanMillQuery m)
    => m ActiveAccounts
activeAccountsData = do
    today <- currentDay
    let interval = beginningOfPrev2Month today ... pred today
    fpm <- personioPlanmillMap
    let uidLookup = Map.fromList
          [ (pmu ^. PM.identifier, p)
          | (p, pmu) <- toList fpm
          ]
    ActiveAccounts interval . mangle today uidLookup <$>
        (traverse . traverse) (perEmployee interval) fpm
  where
    mangle
        :: Day
        -> Map PM.UserId P.Employee
        -> HashMap FUM.Login (P.Employee, (PM.User, Map PM.AccountId (Day, NDT 'Hours Centi, PM.Account)))
        -> Map PM.AccountId ActiveAccount
    mangle today uidLookup xs = Map.fromListWith app
        [ pair accId ActiveAccount
              { aaName      = PM.saName acc
              , aaOwner     = do
                  oid   <- PM.saOwner acc
                  owner <- uidLookup ^? ix oid
                  guard $ P.employeeIsActive today owner
                  owner ^. P.employeeEmail
              , aaEmployees = Map.singleton login $ mkActiveEmployee d h pe pmu
              }
        | (login, (pe, (pmu, ys))) <- HM.toList xs
        , (accId, (d, h, acc)) <- Map.toList ys
        ]
      where
        app (ActiveAccount a o b) (ActiveAccount _ _  b') =
            ActiveAccount a o $ Map.union b b'

        pair = (,)

    perEmployee :: Interval Day -> PM.User -> m (PM.User, Map PM.AccountId (Day, NDT 'Hours Centi, PM.Account))
    perEmployee interval pmu = do
        let uid = pmu ^. PM.identifier
        trs <- PMQ.timereports interval uid
        let projIds = map swap $ Map.toList $ Map.fromListWith (\(a1,b1) (a2,b2) -> (max a1 a2, b1 + b2))
                      [(i, (PM.trStart tr, ndtConvert' $ PM.trAmount tr)) | tr <- toList trs, Just i <- [PM.trProject tr]]
        projects <- (traverse . traverse) PMQ.project projIds
        let accIds = Map.fromListWith (\(a1,b1) (a2,b2) -> (max a1 a2, b1 + b2))
                     [(i, (d,h)) | ((d, h), pr) <- projects, Just i <- [PM._pAccount pr]]
        accounts <- ifor accIds $ \i (d,h) -> (,,) d h <$> PMQ.account i
        return (pmu, accounts)

instance ToHtml ActiveAccounts where
    toHtmlRaw = toHtml
    toHtml = toHtml . activeAccountsRender

activeAccountsRender :: ActiveAccounts -> HtmlPage "active-accounts"
activeAccountsRender (ActiveAccounts i xs) = page_ "Active accounts" $ do
    h1_ "Active accounts"
    p_ $ do
        "generated from marked hours on "
        toHtml $ show $ inf i
        " ... "
        toHtml $ show $ sup i
    table_ $ do
        thead_ $ tr_ $ do
            th_ "Account"
            th_ "Account PM#"
            th_ "Owner"
            th_ "Employee name"
            th_ "Employee PM#"
            th_ [ title_ "Most recent active day" ] "Day"
            th_ [ title_ "Hours done to account during the interval"] "Hours"
            th_ "Email"

        tbody_ $ ifor_ xs $ \accId (ActiveAccount acc owner employees) -> rows
            [ toHtml acc, toHtml accId, traverse_ toHtml owner ]
            $ flip Map.mapWithKey employees $ \_login ActiveEmployee {..} -> do
                td_ $ toHtml aeName
                td_ $ traverse_ toHtml aeEmail
                td_ $ toHtml $ show aeDay
                td_ $ toHtml $ aeHours
                td_ $ toHtml aePlanmillId

rows :: (Monad m, Foldable f) => [HtmlT m ()] -> f (HtmlT m ()) -> HtmlT m ()
rows x ys = case toList ys of
    []      -> tr_ $ traverse_ td_ x >> td_ mempty
    (y:ys') -> do
        tr_ $ traverse_ (td_ [ rowspan_ $ textShow $ succ $ length ys' ]) x >> y
        for_ ys' $ \y' -> tr_ y'
