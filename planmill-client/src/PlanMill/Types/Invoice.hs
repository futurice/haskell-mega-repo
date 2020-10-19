{-# LANGUAGE OverloadedStrings #-}
module PlanMill.Types.Invoice where

import PlanMill.Internal.Prelude
import PlanMill.Types.Identifier (HasIdentifier (..), Identifier)

type InvoiceId = Identifier InvoiceData
type InvoiceDatas = Vector InvoiceData

data InvoiceData = InvoiceData
  { _invoiceId      :: !InvoiceId
  , _invoiceNumber  :: !(Maybe Int)
  , _invoiceDate    :: !Text
  , _invoiceDueDate :: !UTCTime
  , _invoiceCreated :: !UTCTime
  } deriving (Generic)

instance AnsiPretty InvoiceData

instance FromJSON InvoiceData where
  parseJSON = withObject "invoice" $ \obj ->
    InvoiceData
    <$> obj .: "id"
    <*> obj .:? "invoiceNumber"
    <*> obj .: "invoiceDate"
    <*> obj .: "dueDate"
    <*> obj .: "created"
