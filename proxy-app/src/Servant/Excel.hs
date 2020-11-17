{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Servant.Excel
    ( EXCEL
    , ExcelBS (..)
    ) where

import Data.Swagger     (NamedSchema (..), ToSchema (..))
import Futurice.Prelude
import Prelude ()
import Servant.API      (Accept (..), MimeRender (..), MimeUnrender (..))

import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Media   as M

data EXCEL -- deriving Typeable

-- | Wrapper around lazy 'LBS.ByteString'.
newtype ExcelBS = ExcelBS { getExcelBS :: LBS.ByteString }
  deriving (Eq, Ord, Show, Typeable)

instance ToSchema ExcelBS where
    declareNamedSchema _ = pure $ NamedSchema (Just "Excel file") mempty

-- | @application/vnd.ms-excel@
instance Accept EXCEL where
    contentType _ = "application" M.// "vnd.ms-excel"

instance MimeRender EXCEL ExcelBS where
    mimeRender _ = getExcelBS

instance MimeUnrender EXCEL ExcelBS  where
    mimeUnrender _ = pure . ExcelBS
