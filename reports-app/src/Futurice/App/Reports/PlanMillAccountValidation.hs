{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Futurice.App.Reports.PlanMillAccountValidation (
    -- * Report
    pmAccountValidationData,
    -- * Types
    PMAccountValidation (..),
    ) where

import Control.Lens              (traverseOf_)
import Data.Set.Lens             (setOf)
import Data.Tuple                (swap)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Lucid.Foundation
import Futurice.Prelude
import Numeric.Interval.NonEmpty (Interval, (...))
import Prelude ()

import qualified Data.Map.Strict  as Map
import qualified Data.Set         as Set
import qualified Personio         as P
import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ

data PMAccount = PMAccount
    { accId    :: PM.AccountId
    , accName  :: !Text
    , accOwner :: !(Maybe P.Employee)
    }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

deriveGeneric ''PMAccount
instance ToSchema PMAccount where declareNamedSchema = sopDeclareNamedSchema
deriveVia [t| ToJSON PMAccount   `Via` Sopica PMAccount |]
deriveVia [t| FromJSON PMAccount `Via` Sopica PMAccount |]

data PMAccountValidation = PMAccountValidation
    { pmavWithoutOwner :: [PMAccount]
    , pmavSameName     :: [NonEmpty PMAccount]
    }
  deriving stock (Show, Generic)
  deriving anyclass (NFData)

deriveGeneric ''PMAccountValidation
instance ToSchema PMAccountValidation where declareNamedSchema = sopDeclareNamedSchema
deriveVia [t| ToJSON PMAccountValidation   `Via` Sopica PMAccountValidation |]
deriveVia [t| FromJSON PMAccountValidation `Via` Sopica PMAccountValidation |]

pmAccountValidationData
    :: forall m. (MonadTime m, MonadPersonio m, MonadPlanMillQuery m)
    => m PMAccountValidation
pmAccountValidationData = do
    today <- currentDay
    let interval = beginningOfPrev2Month today ... pred today
    fpm <- personioPlanmillMap
    let uidLookup = Map.fromList
          [ (pmu ^. PM.identifier, p)
          | (p, pmu) <- toList fpm
          ]
    accountIds <- mconcat <$> traverse (perEmployee interval) (fpm ^.. folded . _2)
    accounts' <- traverse PMQ.account (toList accountIds)
    let accounts = map (toPMAccount uidLookup) accounts'

    let withoutOwner =
            [ acc
            | acc <- accounts
            , maybe True (not . P.employeeIsActive today) (accOwner acc)
            ]

    let sameName =
          [ acc :| accs
          | acc : accs <- toList $ Map.fromListWith (++)
              [ (accName acc, [acc])
              | acc <- accounts
              ]
          , 1 < length (setOf (folded . getter accOwner . _Just . P.employeeId) (acc : accs))
          ]

    return PMAccountValidation
        { pmavWithoutOwner = withoutOwner
        , pmavSameName     = sameName
        }
  where
    toPMAccount :: Map PM.UserId P.Employee ->  PM.Account -> PMAccount
    toPMAccount uidLookup acc = PMAccount
        { accId    = acc ^. PM.identifier
        , accName  = PM.saName acc
        , accOwner = do
            oid   <- PM.saOwner acc
            uidLookup ^? ix oid
        }

    perEmployee :: Interval Day -> PM.User -> m (Set PM.AccountId)
    perEmployee interval pmu = do
        let uid = pmu ^. PM.identifier
        trs <- PMQ.timereports interval uid
        let projIds = map swap $ Map.toList $ Map.fromListWith max
              [ (i, PM.trStart tr)
              | tr <- toList trs
              , Just i <- [PM.trProject tr]
              ]
        projects <- (traverse . traverse) PMQ.project projIds
        let accIds = Set.fromList
              [ i
              | (_, pr) <- projects
              , Just i  <- [PM._pAccount pr]
              ]
        return accIds

instance ToHtml PMAccountValidation where
    toHtmlRaw = toHtml
    toHtml = toHtml . pmAccountValidationRender

pmAccountValidationRender :: PMAccountValidation -> HtmlPage "planmill-account-validation"
pmAccountValidationRender pmav = page_ "PlanMill account validation" $ fullRow_ $ do
    h1_ "PlanMill account validation"

    h2_ "Accounts without active owner"
    table_ $ do
        thead_ $ tr_ $ do
            th_ "Account"
            th_ "Account PM#"
            th_ "Owner"
            th_ "Owner Personio#"

        tbody_ $ for_ (pmavWithoutOwner pmav) $ \acc -> tr_ $ do
            td_ $ toHtml $ accName acc
            td_ $ toHtml $ accId acc
            td_ $ traverseOf_  (_Just . P.employeeFullname) toHtml (accOwner acc)
            td_ $ traverseOf_  (_Just . P.employeeId) toHtml (accOwner acc)

    h2_ "Accounts with same name (but different owners)"
    table_ $ do
        thead_ $ tr_ $  do
            th_ "Account"
            th_ "Account PM#"
            th_ "Owners"

        tbody_ $ for_ (pmavSameName pmav) $ \(acc :| accs) -> tr_ $ do
            td_ $ toHtml $ accName acc
            td_ $ toHtml $ accId acc
            td_ $ ul_ $ for_ (acc : accs) $ \acc' ->
                li_ $ do
                    traverseOf_  (_Just . P.employeeFullname) toHtml (accOwner acc')
                    " ( account# "
                    toHtml $ accId acc'
                    " )"
