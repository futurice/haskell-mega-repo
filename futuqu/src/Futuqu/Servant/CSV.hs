{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Futuqu.Servant.CSV (
    CSVFraming,
    CSVStreaming,
    ) where

import Futurice.Prelude
import Prelude ()
import Servant.API
import Servant.CSV.Cassava (CSV)

import qualified Data.ByteString.Builder as B
import qualified Data.Csv                as Csv
import qualified Data.Csv.Builder        as Csv
import qualified Servant.Types.SourceT   as S

-------------------------------------------------------------------------------
-- Framing
-------------------------------------------------------------------------------

data CSVFraming a

instance Csv.DefaultOrdered a => FramingRender (CSVFraming a) where
    framingRender _ f = S.mapStepT $ \steps ->
        S.Yield headerBS (fmap f steps)
      where
        headerBS
            = B.toLazyByteString (Csv.encodeHeader (Csv.headerOrder (undefined :: a)))

-------------------------------------------------------------------------------
-- Streaming
-------------------------------------------------------------------------------

data CSVStreaming

instance Accept CSVStreaming where
    contentTypes _ = contentTypes (Proxy :: Proxy CSV)

instance Csv.ToRecord a => MimeRender CSVStreaming a where
    mimeRender _ = B.toLazyByteString . Csv.encodeRecord
