{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
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
    , accName :: !Text
    , accType :: !Text -- TODO, make EnumTextValue
    -- NOTE: there's account owner, but that information is shallow
    -- TODO: add data as needed
    }
  deriving stock (Eq, Ord, Show, GhcGeneric)
  deriving anyclass (NFData, SopGeneric, HasDatatypeInfo)

deriveVia [t| ToJSON Account         `Via` Sopica Account |]
deriveVia [t| FromJSON Account       `Via` Sopica Account |]
deriveVia [t| DefaultOrdered Account `Via` Sopica Account |]
deriveVia [t| ToRecord Account       `Via` Sopica Account |]
deriveVia [t| ToNamedRecord Account  `Via` Sopica Account |]

instance ToSchema Account where declareNamedSchema = sopDeclareNamedSchema

-------------------------------------------------------------------------------
-- Endpoint
-------------------------------------------------------------------------------

accountsData
    :: (MonadPlanMillQuery m)
    => m [Account]
accountsData = do
    prjs  <- PMQ.projects
    let accIds = setOf (folded . getter PM.pAccount . _Just) prjs
    accs  <- traverse PMQ.account (toList accIds)
    types <- PMQ.allEnumerationValues Proxy Proxy
    return $ sortOn accName $ map (convert types) accs
  where
    convert types a = Account
        { accAccountId = a ^. PM.identifier
        , accName      = PM.saName a
        , accType      = fromMaybe "-" $ types ^? ix (PM.saType a)
        }
