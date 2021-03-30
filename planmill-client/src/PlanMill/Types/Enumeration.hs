{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Copyright : (c) 2015 Futurice Oy
-- License   : BSD3
-- Maintainer: Oleg Grenrus <oleg.grenrus@iki.fi>
module PlanMill.Types.Enumeration (
    EnumDesc(..),
    SomeEnumDesc(..),
    EnumValue(..),
    ) where

import PlanMill.Internal.Prelude

import Futurice.Constraint.ForallSymbol (Dict (..), ForallFSymbol (..))
import Futurice.EnvConfig
import Futurice.Reflection.TypeLits     (reifyTypeableSymbol)
import GHC.TypeLits                     (KnownSymbol, Symbol, symbolVal)

import qualified Data.HashMap.Strict as HM
import qualified Data.IntMap.Strict  as IM
import qualified Data.Text           as T

newtype EnumDesc (k :: Symbol) = EnumDesc (IntMap Text)
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

--instance Hashable EnumDesc
instance NFData (EnumDesc k)
instance AnsiPretty (EnumDesc k)
instance Binary (EnumDesc k)

instance KnownSymbol k => FromJSON (EnumDesc k) where
    parseJSON = withObject "Enum description" $ \obj ->
        EnumDesc . getIM <$> obj .: k'
      where
        k' :: Text
        k' = T.pack $ "Enumeration values." <> symbolVal (Proxy :: Proxy k)

instance ForallFSymbol Eq         EnumDesc where instFSymbol = Dict
instance ForallFSymbol NFData     EnumDesc where instFSymbol = Dict
instance ForallFSymbol AnsiPretty EnumDesc where instFSymbol = Dict
instance ForallFSymbol Binary     EnumDesc where instFSymbol = Dict
instance ForallFSymbol FromJSON   EnumDesc where instFSymbol = Dict
instance ForallFSymbol Show       EnumDesc where instFSymbol = Dict
instance ForallFSymbol Typeable   EnumDesc where
  instFSymbol = dict
     where
       dict :: forall k. KnownSymbol k => Dict (Typeable (EnumDesc k))
       dict = reifyTypeableSymbol (Proxy :: Proxy k) Dict

data SomeEnumDesc where
    MkSomeEnumDesc :: KnownSymbol k => EnumDesc k -> SomeEnumDesc

newtype EnumValue entity (field :: Symbol) = EnumValue Int
    deriving (Eq, Ord, Show, Read, Generic, Typeable)

instance Hashable (EnumValue entity field)
instance NFData (EnumValue entity field)
instance AnsiPretty (EnumValue entity field)
instance Binary (EnumValue entity field)

instance FromJSON (EnumValue entity field) where
    parseJSON = fmap EnumValue . parseJSON

instance ToJSON (EnumValue entity field) where
    toJSON (EnumValue v) = toJSON v

instance FromEnvVarList (EnumValue entity field) where
    fromEnvVarList = fmap2 EnumValue . fromEnvVarList where fmap2 = fmap . fmap

-------------------------------------------------------------------------------
-- IntMap from Object
-------------------------------------------------------------------------------

newtype IM a = IM { getIM :: IntMap a }


-- | Filtering non-Int:s away as meta json sometimes have useless "optgroup"
-- string values
instance FromJSON a => FromJSON (IM a) where
    parseJSON v = IM . toIM <$> parseJSON v
      where
        toIM :: HashMap String a -> IntMap a
        toIM xs = IM.fromList
            [ (i, a)
            | (s, a) <- HM.toList xs
            , Just i <- [readMaybe s]
            ]

-------------------------------------------------------------------------------
-- Template Haskell
-------------------------------------------------------------------------------

deriveGeneric ''EnumDesc
deriveGeneric ''EnumValue

instance (KnownSymbol k) => Structured (EnumDesc k)
instance ForallFSymbol Structured EnumDesc where
    instFSymbol = Dict
instance (Typeable entity, KnownSymbol field) => Structured (EnumValue entity field)
