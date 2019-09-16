{-# LANGUAGE ConstraintKinds        #-}
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
-- | This module is not used.
module Futurice.Integrations.Serv.Contains (
    -- * Contains
    Contains,
    ContainsProof,
    withContainsProof,
    IfContains,
    -- * withServSetC
    withServSetC,
    ) where

import Data.Kind          (Type)
import Data.Proxy         (Proxy (..))
import Data.Type.Equality
import Unsafe.Coerce      (unsafeCoerce)

import Futurice.Integrations.Serv

-------------------------------------------------------------------------------
-- Contains
-------------------------------------------------------------------------------

-- These types will help in error messages.
data HasServ (s :: Serv)
data NoServ (ss :: [Serv])

-- | Constraint htat tells that @s@ is in @ss@.
type Contains (s :: Serv) (ss :: [Serv]) =
    IfContains s ss (HasServ s) (NoServ ss) ~ HasServ s

-------------------------------------------------------------------------------
-- ContainsProof
-------------------------------------------------------------------------------

data ContainsProof (s :: Serv) (ss :: [Serv]) where
    Here  :: SServ s -> ContainsProof s (s ': ss)
    There :: LessThanProof (ServNat z) (ServNat s)  -- "inequality" proof
          -> ContainsProof s ss
          -> ContainsProof s (z ': ss)

deriving instance Show (ContainsProof s ss)

-- |
--
-- >>> withServSetC (Proxy :: Proxy '[ ServFD, ServGH ]) (Const []) (\c s (Const ss) -> Const (show (c, s) : ss))
-- Const ["(Here SServFD,SServFD)","(There LTZ (Here SServGH),SServGH)"]
--
withServSetC
    :: forall ss f. ServSet ss
    => Proxy ss
    -> f '[]
    -> (forall s zs. ContainsProof s ss -> SServ s -> f zs -> f (s ': zs))
    -> f ss
withServSetC _ nil cons = go id ssProof where
    go :: (forall s. ContainsProof s zs -> ContainsProof s ss) -> ServSetProof zs -> f zs
    go _ SSEmpty         = nil
    go l (SSSing s)      = cons (l (Here s)) s nil
    go l (SSCons s lt p) = cons (l (Here s)) s (go (\q -> l (There (transLT lt q) q)) p)

transLT :: LessThanProof (ServNat s) (ServNat z) -> ContainsProof s1 (z ': zs) -> LessThanProof (ServNat s) (ServNat s1)
transLT lt (Here _)      = lt
transLT lt (There lt' _) = lessThanTrans lt lt'

-- |
--
-- >>> :kind! IfContains ServGH '[ServFD, ServGH] Int ()
-- ...
-- = Int
--
-- >>> :kind! IfContains ServFUM '[ServFD, ServGH] Int ()
-- ...
-- = ()
--
type family IfContains (s :: Serv) (ss :: [Serv]) (a :: Type) (b :: Type) where
    IfContains s '[]       a b = b
    IfContains s (z ': zs) a b = WithOrdering (CMP (ServNat s) (ServNat z))
        b
        a
        (IfContains s zs a b)
{-
    IfContains s (s ': ss) a b = a
    IfContains s (z ': ss) a b = IfContains s ss a b
-}

-- |
--
-- >>> type Servs = '[ ServFUM, ServFUM6 ]
-- >>> let co = Here sserv :: ContainsProof ServFUM Servs
-- >>> withContainsProof (Proxy :: Proxy Int)  co
-- Refl
--
withContainsProof :: forall a b s ss. Proxy b -> ContainsProof s ss -> IfContains s ss a b :~: a
withContainsProof _ = go where
    go :: ContainsProof s zz -> IfContains s zz a b :~: a
    go (Here  s)    = case cmpReflServ s of Refl -> Refl
    go (There lt p) = next lt p (go p)

    next :: forall z zs. LessThanProof (ServNat z) (ServNat s) -> ContainsProof s zs -> IfContains s zs a b :~: a -> IfContains s (z : zs) a b :~: a
    next lt _ r = case lemma (Proxy :: Proxy zs) lt of
        r' -> trans r' r

    -- if    ServNat y  <  ServNat z
    --    => CMP (ServNat z) (ServNat y) ~ 'GT
    lemma :: Proxy zs -> LessThanProof (ServNat y) (ServNat s) -> IfContains s (y ': zs) a b :~: IfContains s zs a b
    lemma _ lt = case lessThanCmpGT lt of Refl -> Refl

{-# NOINLINE withContainsProof #-}

{-# RULES "withContainsProof" forall p c. withContainsProof p c = trustMe #-}
trustMe :: forall x y. x :~: y
trustMe = unsafeCoerce (Refl :: () :~: ())

cmpReflServ :: SServ s -> CMP (ServNat s) (ServNat s) :~: 'EQ
cmpReflServ SServFD   = Refl
cmpReflServ SServFUM  = Refl
cmpReflServ SServFUM6 = Refl
cmpReflServ SServGH   = Refl
cmpReflServ SServGO   = Refl
cmpReflServ SServOK   = Refl
cmpReflServ SServPE   = Refl
cmpReflServ SServPM   = Refl
cmpReflServ SServPO   = Refl

-- $setup
--
-- >>> :set -XDataKinds
-- >>> import Data.Functor.Const (Const (..))
