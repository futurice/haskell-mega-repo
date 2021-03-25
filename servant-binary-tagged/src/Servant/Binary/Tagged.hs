{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- |
-- Module      :  Servant.Binary.Tagged
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- An @BINARYTAGGED@ empty data type with `MimeRender` instances for @binary@
-- and @binary-tagged@ 'HasStructuralInfo', 'HasSemanticVersion' and 'Binary' a
-- classes.  You should only need to import this module for it's instances and
-- the `BINARYTAGGED` datatype.:
--
-- >>> type BinaryTaggedGET a = Get '[BINARYTAGGED] a
module Servant.Binary.Tagged where

import Data.Binary        (Binary)
import Data.Binary.Tagged (Structured, structuredDecode, structuredEncode)
import Servant.API        (Accept (..), MimeRender (..), MimeUnrender (..))

import qualified Codec.Compression.GZip as GZip
import qualified Network.HTTP.Media     as M

data BINARYTAGGED -- deriving Typeable

-- | @application/x-yaml@
instance Accept BINARYTAGGED where
    contentType _ = "application" M.// "gzip-binary-tagged"

-- | `taggedEncode`
instance (Structured a, Binary a) => MimeRender BINARYTAGGED a where
    mimeRender _ = GZip.compress . structuredEncode

-- | `taggedDecodeOrFail`
instance  (Structured a, Binary a) => MimeUnrender BINARYTAGGED a where
    -- there might be some trailing data, but we don't care about it atm.
    mimeUnrender _ = pure . structuredDecode . GZip.decompress
