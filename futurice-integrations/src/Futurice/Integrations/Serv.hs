{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
module Futurice.Integrations.Serv (
    -- * Services
    Serv (..),
    ServNat (..),
    -- ** Singleton
    SServ (..), ServI (..),
    -- * Service Sets
    ServSet (..),
    ServSetProof (..),
    -- * Using Service Sets
    withServSet,
    -- * fin extras
    LessThan (..),
    LessThanProof (..),
    ) where

import Data.Functor.Classes (showsUnaryWith)
import Data.Kind            (Type)
import Data.Proxy           (Proxy (..))

import qualified Data.Type.Nat as N

-------------------------------------------------------------------------------
-- LessThan
-------------------------------------------------------------------------------

data LessThanProof :: N.Nat -> N.Nat -> Type where
    LTZ :: LessThanProof 'N.Z ('N.S m)
    LTS :: LessThanProof n m -> LessThanProof ('N.S n) ('N.S m)

deriving instance Show (LessThanProof n m)

-- | GHC can solve this for us!
--
-- >>> ltProof :: LessThanProof N.Nat0 N.Nat4
-- LTZ
--
-- >>> ltProof :: LessThanProof N.Nat2 N.Nat4
-- LTS (LTS LTZ)
--
-- >>> ltProof :: LessThanProof N.Nat3 N.Nat3
-- ...
-- ...error...
-- ...
--
class LessThan (n :: N.Nat) (m :: N.Nat) where
    ltProof :: LessThanProof n m

instance m ~ 'N.S m' => LessThan 'N.Z m where
    ltProof = LTZ

instance LessThan n m => LessThan ('N.S n) ('N.S m) where
    ltProof = LTS ltProof

-------------------------------------------------------------------------------
-- Services
-------------------------------------------------------------------------------

-- | Service we integrate to
--
-- /Note:/ constructors are in the alphabetical order.
data Serv
    = ServFD    -- ^ flowdock
    | ServFUM   -- ^ fum
    | ServFUM6  -- ^ fum-carbon
    | ServGH    -- ^ github
    | ServPE    -- ^ personio
    | ServPM    -- ^ planmill
  deriving (Show)

type ServFD   = 'ServFD
type ServFUM  = 'ServFUM
type ServFUM6 = 'ServFUM6
type ServGH   = 'ServGH
type ServPE   = 'ServPE
type ServPM   = 'ServPM

-- | Serv to nat
--
-- >>> :kind! ServNat ServFUM6
-- ServNat ServFUM6 :: N.Nat
-- = N.Nat2
--
type family ServNat (s :: Serv) = (n :: N.Nat) | n -> s where
    ServNat 'ServFD   = N.Nat0
    ServNat 'ServFUM  = N.Nat1
    ServNat 'ServFUM6 = N.Nat2
    ServNat 'ServGH   = N.Nat3
    ServNat 'ServPE   = N.Nat4
    ServNat 'ServPM   = N.Nat5

-------------------------------------------------------------------------------
-- Services Singleton
-------------------------------------------------------------------------------

data SServ :: Serv -> Type where
    SServFD   :: SServ 'ServFD
    SServFUM  :: SServ 'ServFUM
    SServFUM6 :: SServ 'ServFUM6
    SServGH   :: SServ 'ServGH
    SServPE   :: SServ 'ServPE
    SServPM   :: SServ 'ServPM

deriving instance Show (SServ s)

-- |
--
-- >>> sserv :: SServ ServGH
-- SServGH
--
class    ServI (s :: Serv) where sserv :: SServ s
instance ServI 'ServFD     where sserv = SServFD
instance ServI 'ServFUM    where sserv = SServFUM
instance ServI 'ServFUM6   where sserv = SServFUM6
instance ServI 'ServGH     where sserv = SServGH
instance ServI 'ServPE     where sserv = SServPE
instance ServI 'ServPM     where sserv = SServPM

-------------------------------------------------------------------------------
-- Services Set
-------------------------------------------------------------------------------

data ServSetProof :: [Serv] -> Type where
    SSEmpty :: ServSetProof '[]
    SSSing  :: SServ s -> ServSetProof '[ s ]
    SSCons  :: SServ s
            -> LessThanProof (ServNat s) (ServNat z)
            -> ServSetProof (z ': zs)
            -> ServSetProof (s ': z ': zs)

deriving instance Show (ServSetProof ss)

-- | As 'LessThan', GHC can tell which 'Serv' lists are ordered and contain
-- only unique elements.
--
-- This proof also acts as way "singleton" of a list.
--
-- >>> ssProof :: ServSetProof '[]
-- SSEmpty
--
-- >>> ssProof :: ServSetProof '[ ServGH ]
-- SSSing SServGH
--
-- >>> ssProof :: ServSetProof '[ ServFUM, ServFUM6 ]
-- SSCons SServFUM (LTS LTZ) (SSSing SServFUM6)
--
-- Sanity test: all services
--
-- >>> ssProof :: ServSetProof '[ ServFD, ServFUM, ServFUM6, ServGH, ServPE, ServPM ]
-- SSCons SServFD LTZ (SSCons SServFUM (LTS LTZ) (SSCons SServFUM6 (LTS (LTS LTZ)) (SSCons SServGH (LTS (LTS (LTS LTZ))) (SSCons SServPE (LTS (LTS (LTS (LTS LTZ)))) (SSSing SServPM)))))
--
-- Error case: duplicate
--
-- >>> ssProof :: ServSetProof '[ ServFD, ServFD ]
-- ...
-- ...error...
-- ...
--
-- Error case: out-of-order
--
-- >>> ssProof :: ServSetProof '[ ServFUM, ServFD ]
-- ...
-- ...error...
-- ...
--
class ServSet (ss :: [Serv]) where
    ssProof :: ServSetProof ss

instance ServSet '[] where
    ssProof = SSEmpty

instance ServSetAux s ss => ServSet (s ': ss) where
    ssProof = ssProofAux

-- | An auxiliary class, to avoid overlapping instances.
class ServSetAux s ss where
    ssProofAux :: ServSetProof (s ': ss)

instance ServI s => ServSetAux s '[] where
    ssProofAux = SSSing sserv

instance
    ( ServI s
    , LessThan (ServNat s) (ServNat z)
    , ServSet (z ': zs)
    ) => ServSetAux s (z ': zs) where
    ssProofAux = SSCons sserv ltProof ssProof

-------------------------------------------------------------------------------
-- "Catamorphism"
-------------------------------------------------------------------------------

-- | Fold over 'ServSet'.
-- We don't use the set property here, as in some cases we don't need it on the value level: types have done their job.
--
-- >>> import Data.Functor.Const (Const (..))
-- >>> withServSet (Proxy :: Proxy '[ ServFD, ServGH ]) (Const []) (\s (Const ss) -> Const (show s : ss))
-- Const ["SServFD","SServGH"]
--
-- Error case:
--
-- >>> withServSet (Proxy :: Proxy '[ ServFD, ServFD ]) (Const []) (\s (Const ss) -> Const (show s : ss))
-- ...
-- ...error...
-- ...
--
withServSet
    :: forall ss f. ServSet ss
    => Proxy ss
    -> f '[]
    -> (forall s zs. SServ s -> f zs -> f (s ': zs))
    -> f ss
withServSet _ nil cons = go ssProof where
    go :: ServSetProof zs -> f zs
    go SSEmpty        = nil
    go (SSSing s)     = cons s nil
    go (SSCons s _ p) = cons s (go p)

-- $setup
--
-- >>> :set -XDataKinds

