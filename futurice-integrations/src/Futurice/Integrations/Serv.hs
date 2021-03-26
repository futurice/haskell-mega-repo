{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE EmptyCase              #-}
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
    AllServs,
    ServNat,
    ServFUM, ServFUM6, ServGH, ServGO, ServOK, ServPE, ServPK, ServPM, ServPO, ServSL,
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
    -- ** Comparison
    CMP,
    WithOrdering,
    -- ** Lemmas
    lessThanReflAbsurd,
    lessThanCmpGT,
    lessThanTrans,
    ) where

import Data.Kind          (Type)
import Data.Type.Equality

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

lessThanReflAbsurd :: N.SNat n -> LessThanProof n n -> a
lessThanReflAbsurd N.SZ p       = case p of {}
lessThanReflAbsurd N.SS (LTS p) = lessThanReflAbsurd N.snat p

lessThanTrans
    :: LessThanProof n m
    -> LessThanProof m p
    -> LessThanProof n p
lessThanTrans LTZ      (LTS _)   = LTZ
lessThanTrans (LTS lt) (LTS lt') = LTS (lessThanTrans lt lt')

type family CMP (n :: N.Nat) (m :: N.Nat) :: Ordering where
    CMP  'N.Z     'N.Z    = 'EQ
    CMP  'N.Z    ('N.S m) = 'LT
    CMP ('N.S n)  'N.Z    = 'GT
    CMP ('N.S n) ('N.S m) = CMP n m

type family WithOrdering (o :: Ordering) a b c :: Type where
    WithOrdering 'LT a b c = a
    WithOrdering 'EQ a b c = b
    WithOrdering 'GT a b c = c

lessThanCmpGT :: LessThanProof n m -> CMP m n :~: 'GT
lessThanCmpGT LTZ      = Refl
lessThanCmpGT (LTS lt) = case lessThanCmpGT lt of Refl -> Refl

-------------------------------------------------------------------------------
-- Services
-------------------------------------------------------------------------------

-- | Service we integrate to
--
-- /Note:/ constructors are in the alphabetical order.
data Serv
    = ServFUM   -- ^ fum
    | ServFUM6  -- ^ fum-carbon
    | ServGH    -- ^ github
    | ServGO    -- ^ google
    | ServOK    -- ^ okta
    | ServPE    -- ^ personio
    | ServPK    -- ^ peakon
    | ServPM    -- ^ planmill
    | ServPO    -- ^ power
    | ServSL    -- ^ slack
  deriving (Show)

type AllServs = '[ ServFUM, ServFUM6, ServGH, ServGO, ServOK, ServPE, ServPK, ServPM, ServPO, ServSL ]

type ServFUM  = 'ServFUM
type ServFUM6 = 'ServFUM6
type ServGH   = 'ServGH
type ServGO   = 'ServGO
type ServOK   = 'ServOK
type ServPE   = 'ServPE
type ServPK   = 'ServPK
type ServPM   = 'ServPM
type ServPO   = 'ServPO
type ServSL   = 'ServSL

-- | Serv to nat
--
-- >>> :kind! ServNat ServFUM6
-- ServNat ServFUM6 :: N.Nat
-- = N.Nat2
--
type Nat10 = 'N.S N.Nat9

type family ServNat (s :: Serv) = (n :: N.Nat) | n -> s where
    ServNat 'ServFUM  = N.Nat1
    ServNat 'ServFUM6 = N.Nat2
    ServNat 'ServGH   = N.Nat3
    ServNat 'ServGO   = N.Nat4
    ServNat 'ServOK   = N.Nat5
    ServNat 'ServPE   = N.Nat6
    ServNat 'ServPK   = N.Nat7
    ServNat 'ServPM   = N.Nat8
    ServNat 'ServPO   = N.Nat9
    ServNat 'ServSL   = Nat10

-------------------------------------------------------------------------------
-- Services Singleton
-------------------------------------------------------------------------------

data SServ :: Serv -> Type where
    SServFUM  :: SServ 'ServFUM
    SServFUM6 :: SServ 'ServFUM6
    SServGH   :: SServ 'ServGH
    SServGO   :: SServ 'ServGO
    SServOK   :: SServ 'ServOK
    SServPE   :: SServ 'ServPE
    SServPK   :: SServ 'ServPK
    SServPM   :: SServ 'ServPM
    SServPO   :: SServ 'ServPO
    SServSL   :: SServ 'ServSL

deriving instance Show (SServ s)

-- |
--
-- >>> sserv :: SServ ServGH
-- SServGH
--
class    ServI (s :: Serv) where sserv :: SServ s
instance ServI 'ServFUM    where sserv = SServFUM
instance ServI 'ServFUM6   where sserv = SServFUM6
instance ServI 'ServGH     where sserv = SServGH
instance ServI 'ServGO     where sserv = SServGO
instance ServI 'ServOK     where sserv = SServOK
instance ServI 'ServPE     where sserv = SServPE
instance ServI 'ServPK     where sserv = SServPK
instance ServI 'ServPM     where sserv = SServPM
instance ServI 'ServPO     where sserv = SServPO
instance ServI 'ServSL     where sserv = SServSL

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
-- >>> ssProof :: ServSetProof '[ ServFUM, ServFUM6, ServGH, ServGO, ServOK, ServPE, ServPM, ServPO, ServSL ]
-- SSCons SServFUM (LTS LTZ) (SSCons SServFUM6 (LTS (LTS LTZ)) (SSCons SServGH (LTS (LTS (LTS LTZ))) (SSCons SServGO (LTS (LTS (LTS (LTS LTZ)))) (SSCons SServOK (LTS (LTS (LTS (LTS (LTS LTZ))))) (SSCons SServPE (LTS (LTS (LTS (LTS (LTS (LTS LTZ)))))) (SSCons SServPM (LTS (LTS (LTS (LTS (LTS (LTS (LTS (LTS LTZ)))))))) (SSCons SServPO (LTS (LTS (LTS (LTS (LTS (LTS (LTS (LTS (LTS LTZ))))))))) (SSSing SServSL))))))))
--
-- Error case: duplicate
--
-- >>> ssProof :: ServSetProof '[ ServFUM, ServFUM ]
-- ...
-- ...error...
-- ...
--
-- Error case: out-of-order
--
-- >>> ssProof :: ServSetProof '[ ServFUM6, ServFUM ]
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
-- >>> withServSet  (Const []) (\s (Const ss) -> Const (show s : ss)) :: Const [String] '[ ServFD, ServGH ]
-- Const ["SServFD","SServGH"]
--
-- Error case:
--
-- >>> withServSet (Const []) (\s (Const ss) -> Const (show s : ss)) :: Const [String] '[ ServFD, ServFD ]
-- ...
-- ...error...
-- ...
--
withServSet
    :: forall ss f. ServSet ss
    => f '[]
    -> (forall s zs. SServ s -> f zs -> f (s ': zs))
    -> f ss
withServSet nil cons = go ssProof where
    go :: ServSetProof zs -> f zs
    go SSEmpty        = nil
    go (SSSing s)     = cons s nil
    go (SSCons s _ p) = cons s (go p)

-- $setup
--
-- >>> :set -XDataKinds
-- >>> import Data.Functor.Const (Const (..))
