{-# LANGUAGE OverloadedStrings #-}
-- | TODO: eventually move to @futurice-prelude@
module Futurice.Daily where

import Data.Ord                  (Down (..))
import Futurice.Prelude
import Prelude ()

import qualified Data.Map.Strict                    as Map
import qualified Data.Function.Step.Discrete.Closed as SF
import qualified Data.Swagger                       as Swagger

newtype Daily v = Daily (SF.SF (Down Day) v)
  deriving stock (Functor, Show, Generic)
  deriving anyclass (NFData)
  deriving newtype (Applicative, Monad)

dailyFromMap :: Eq v => v -> Map Day v -> Daily v
dailyFromMap v m = Daily $ SF.normalise $ SF.SF (Map.mapKeys Down m) v

-- | Like 'dailyFromMap' but use first value of the 'Map' (if exists)
-- as the default value (i.e .before the first value).
dailyFromMap' :: Eq v => v -> Map Day v -> Daily v
dailyFromMap' v m = case Map.lookupMin m of
    Nothing      -> dailyFromMap v m
    Just (_, v') -> dailyFromMap v' m

(!) :: Daily v -> Day -> v
Daily m ! d = m SF.! Down d

-- TODO: write more instances, notably ToJSON/FromJSON

instance Swagger.ToSchema (Daily v) where
    declareNamedSchema _ = pure $ Swagger.NamedSchema (Just "Daily ?") mempty
