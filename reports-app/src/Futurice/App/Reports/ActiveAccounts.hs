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

import Data.Tuple                (swap)
import Futurice.Email            (Email)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Numeric.Interval.NonEmpty (Interval, (...), inf, sup)
import Prelude ()

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as Map
import qualified FUM.Types.Login     as FUM
import qualified Personio            as P
import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ

data ActiveEmployee = ActiveEmployee
    { aeName       :: !Text
    , aeDay        :: !Day
    , aeEmail      :: !(Maybe Email)
    , aePlanmillId :: !PM.UserId
    }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

mkActiveEmployee :: Day -> P.Employee -> PM.User -> ActiveEmployee
mkActiveEmployee d pe pmu = ActiveEmployee
    { aeName       = pe ^. P.employeeFullname
    , aeDay        = d
    , aeEmail      = pe ^. P.employeeEmail
    , aePlanmillId = pmu ^. PM.identifier
    }

deriveGeneric ''ActiveEmployee
instance ToSchema ActiveEmployee where declareNamedSchema = sopDeclareNamedSchema
deriveVia [t| ToJSON ActiveEmployee   `Via` Sopica ActiveEmployee |]
deriveVia [t| FromJSON ActiveEmployee `Via` Sopica ActiveEmployee |]

data ActiveAccount = ActiveAccount
    { aaName      :: !Text
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
    ActiveAccounts interval . mangle <$>
        (traverse . traverse) (perEmployee interval) fpm
  where
    mangle
        :: HashMap FUM.Login (P.Employee, (PM.User, Map PM.AccountId (Day, PM.Account)))
        -> Map PM.AccountId ActiveAccount
    mangle xs = Map.fromListWith app
        [ (accId, ActiveAccount (PM.saName acc) $ Map.singleton login $ mkActiveEmployee d pe pmu)
        | (login, (pe, (pmu, ys))) <- HM.toList xs
        , (accId, (d, acc)) <- Map.toList ys
        ]
      where
        app (ActiveAccount a b) (ActiveAccount _ b') =
            ActiveAccount a $ Map.union b b'

    perEmployee :: Interval Day -> PM.User -> m (PM.User, Map PM.AccountId (Day, PM.Account))
    perEmployee interval pmu = do
        let uid = pmu ^. PM.identifier
        trs <- PMQ.timereports interval uid
        let projIds = map swap $ Map.toList $ Map.fromListWith max
              [ (i, PM.trStart tr)
              | tr <- toList trs
              , Just i <- [PM.trProject tr]
              ]
        projects <- (traverse . traverse) PMQ.project projIds
        let accIds = Map.fromListWith max
              [ (i, d)
              | (d, pr) <- projects
              , Just i  <- [PM.pAccount pr]
              ]
        accounts <- ifor accIds $ \i d -> (,) d <$> PMQ.account i
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
            th_ "Employee name"
            th_ [ title_ "Most recent active day" ] "Day"
            th_ "PlanMill"
            th_ "Email"

        tbody_ $ for_ xs $ \(ActiveAccount acc employees) -> rows
            (toHtml acc)
            $ flip Map.mapWithKey employees $ \_login ActiveEmployee {..} -> do
                td_ $ toHtml aeName
                td_ $ traverse_ toHtml aeEmail
                td_ $ toHtml $ show aeDay
                td_ $ toHtml aePlanmillId

rows :: (Monad m, Foldable f) => HtmlT m () -> f (HtmlT m ()) -> HtmlT m ()
rows x ys = case toList ys of
    []      -> tr_ $ td_ x >> td_ mempty
    (y:ys') -> do
        tr_ $ td_ [ rowspan_ $ textShow $ succ $ length ys' ] x >> y
        for_ ys' $ \y' -> tr_ y'
