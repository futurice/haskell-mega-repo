{-# LANGUAGE DerivingVia #-}
module Futurice.App.Reports.Invoice where

import Data.Time
import Futurice.Generics
import Futurice.Integrations
import Futurice.Prelude
import Prelude ()

import qualified Data.Vector      as V
import qualified PlanMill         as PM
import qualified PlanMill.Queries as PMQ

-------------------------------------------------------------------------------
-- Data
-------------------------------------------------------------------------------

data Invoice = Invoice
  { iid            :: !PM.InvoiceId
  , iinvoiceNumber :: !(Maybe Int)
  , iinvoiceDate   :: !Text
  , idueDate       :: !UTCTime
  , icreated       :: !UTCTime
  } deriving (GhcGeneric, SopGeneric,ToSchema, HasDatatypeInfo, NFData)
    deriving (FromJSON, ToJSON) via (Sopica Invoice)

-------------------------------------------------------------------------------
-- Endpoint
-------------------------------------------------------------------------------

invoiceData :: (MonadPlanMillQuery m) => Maybe Month -> m [Invoice]
invoiceData mmonth = do
    invoicedata <- PMQ.invoices mmonth
    pure $ map toInvoice $ V.toList invoicedata
  where
    toInvoice invoice = Invoice
      { iid            = PM._invoiceId invoice
      , iinvoiceNumber = PM._invoiceNumber invoice
      , iinvoiceDate   = PM._invoiceDate invoice
      , idueDate       = PM._invoiceDueDate invoice
      , icreated       = PM._invoiceCreated invoice
      }
