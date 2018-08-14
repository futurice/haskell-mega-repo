{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Futurice.App.Reports.ActiveAccounts (
    activeAccountsData,
    activeAccountsRender,
    ) where

import Data.Tuple                (swap)
import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Numeric.Interval.NonEmpty (Interval, (...))
import Prelude ()

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict     as Map
import qualified FUM.Types.Login     as FUM
import qualified Personio            as P
import qualified PlanMill            as PM
import qualified PlanMill.Queries    as PMQ

type DataSet = Map PM.AccountId (PM.Account, Map FUM.Login (Day, P.Employee, PM.User))

activeAccountsData
    :: forall m. (MonadTime m, MonadPersonio m, MonadPlanMillQuery m)
    => m DataSet
activeAccountsData = do
    today <- currentDay
    let interval = beginningOfPrev2Month today ... pred today
    fpm <- personioPlanmillMap
    mangle <$> (traverse . traverse) (perEmployee interval) fpm
  where
    mangle
        :: HashMap FUM.Login (P.Employee, (PM.User, Map PM.AccountId (Day, PM.Account)))
        -> DataSet
    mangle xs = Map.fromListWith app
        [ (accId, (acc, Map.singleton login (d, pe, pmu)))
        | (login, (pe, (pmu, ys))) <- HM.toList xs
        , (accId, (d, acc)) <- Map.toList ys
        ]
      where
        app (a, b) (_, b') = (a, Map.union b b')

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

activeAccountsRender :: DataSet -> HtmlPage "active-accounts"
activeAccountsRender xs = page_ "Active accounts" $ table_ $ do
    thead_ $ tr_ $ do
        th_ "Account"
        th_ "Employees"

    tbody_ $ for_ xs $ \(acc, employees) -> tr_ $ do
        td_ $ toHtml $ PM.saName acc
        td_ $ ul_ $ ifor_ employees $ \_login (day, e, pmu) -> li_ $ do
            toHtml $ e ^. P.employeeFullname
            " - "
            toHtml $ show day
            " - PM:"
            toHtml $ pmu ^. PM.identifier
            
