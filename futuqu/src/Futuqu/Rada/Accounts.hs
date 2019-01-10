{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Futuqu.Rada.Accounts where

import Data.Set.Lens         (setOf)
import Futurice.Generics
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Account = Account
    -- identifiers
    { accAccountId :: !PM.AccountId
    -- planmill
    , accName      :: !Text
    , accType      :: !Text -- TODO, make EnumTextValue
    , accOwnerId   :: !(Maybe PM.UserId)
    , accPassive   :: !Text -- TODO, make EnumTextValue
    , accCreated   :: !(Maybe UTCTime)
    , accCreatedBy :: !(Maybe Text)
    }
  deriving stock (Eq, Ord, Show, GhcGeneric)
  deriving anyclass (NFData, SopGeneric, HasDatatypeInfo)
  deriving (ToJSON, FromJSON, DefaultOrdered, ToRecord, ToNamedRecord) via (Sopica Account)

instance ToSchema Account where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Endpoint
-------------------------------------------------------------------------------

accountsData
    :: (MonadPlanMillQuery m, MonadMemoize m)
    => m [Account]
accountsData = do
    prjs  <- PMQ.projects
    let accIds = setOf (folded . getter PM.pAccount . _Just) prjs
    accs  <- traverse PMQ.account (toList accIds)
    types <- PMQ.allEnumerationValues Proxy Proxy
    passive <- PMQ.allEnumerationValues Proxy Proxy
    return $ sortOn accName $ map (convert types passive) accs
  where
    convert types passive a = Account
        { accAccountId = a ^. PM.identifier
        , accName      = PM.saName a
        , accType      = fromMaybe "-" $ types ^? ix (PM.saType a)
        , accOwnerId   = PM.saOwner a
        , accPassive   = fromMaybe "-" $ passive ^? ix (PM.saPassive a)
        , accCreated   = PM.saCreated a
        , accCreatedBy = PM.saCreatedBy a
        }
