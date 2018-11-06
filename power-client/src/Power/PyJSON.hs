{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Power.PyJSON (PYJSON, PyJSON (..)) where

import Data.Aeson.Parser
import Data.Aeson.Types  (FromJSON (..), Parser, Value, listParser, FromJSON1 (..))
import Futurice.Prelude
import Prelude ()
import Servant.API

data PYJSON

instance Accept PYJSON where
    contentTypes _ = contentTypes (Proxy :: Proxy JSON)

instance PyJSON a => MimeUnrender PYJSON a where
    mimeUnrender _ = fmap unwrapPyJSON . mimeUnrender (Proxy :: Proxy JSON)

-------------------------------------------------------------------------------
-- Class
-------------------------------------------------------------------------------

-- | Like 'FromJSON' but especially to parse power outputs.
class PyJSON a where
    parsePyJSON :: Value -> Parser a

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance PyJSON a => PyJSON [a] where
    parsePyJSON = liftParseJSON parsePyJSON (listParser parsePyJSON)

-------------------------------------------------------------------------------
-- Wrapping
-------------------------------------------------------------------------------

newtype WrappedPyJSON a = WrapPyJSON { unwrapPyJSON :: a }

instance PyJSON a =>  FromJSON (WrappedPyJSON a) where
    parseJSON = fmap WrapPyJSON . parsePyJSON
