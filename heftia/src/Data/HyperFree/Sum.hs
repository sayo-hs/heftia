{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.HyperFree.Sum where

import Control.Effect.Class (NopI, type (~>))
import Control.Freer (Freer, interpretFreer, liftIns, transformFreer)
import Control.HyperFree (
    ASigClass (ASigClass),
    GetSigClass,
    HyperFree,
    HyperFunctor,
    LiftIns (LiftIns),
    SigClass,
    hyfmap,
    overHyperFree,
    unHyperFree,
    unliftIns,
    type (#),
 )
import Data.Free.Sum (caseSum, pattern L1, pattern R1, type (+))
import Data.Functor.Contravariant (Contravariant)
import Data.Kind (Type)
import GHC.Generics (Generic, Generic1)

infixr 5 #+#, #+, +#, :+:

newtype
    (:+:)
        (e1 :: SigClass)
        (e2 :: SigClass)
        (h :: ASigClass -> Type -> Type)
        (e :: ASigClass)
        (a :: Type) = SumH {unSumH :: (e1 h e + e2 h e) a}
    deriving newtype (Generic, Generic1, Contravariant, Eq, Ord, Functor, Foldable)
    deriving stock (Traversable, Read, Show)

type (e1 :: SigClass) +# (e2 :: ASigClass) = 'ASigClass (e1 :+: GetSigClass e2)
type (e1 :: ASigClass) #+ (e2 :: SigClass) = 'ASigClass (GetSigClass e1 :+: e2)
type (e1 :: ASigClass) #+# (e2 :: ASigClass) = 'ASigClass (GetSigClass e1 :+: GetSigClass e2)

sumH :: (e1 h e a -> r) -> (e2 h e a -> r) -> (e1 :+: e2) h e a -> r
sumH f g = caseSum f g . unSumH
{-# INLINE sumH #-}

swapSumH :: (e1 :+: e2) h e ~> (e2 :+: e1) h e
swapSumH = SumH . sumH R1 L1
{-# INLINE swapSumH #-}

assocLSumH :: (e1 :+: (e2 :+: e3)) h e ~> ((e1 :+: e2) :+: e3) h e
assocLSumH = SumH . sumH (L1 . SumH . L1) (sumH (L1 . SumH . R1) R1)

assocRSumH :: ((e1 :+: e2) :+: e3) h e ~> (e1 :+: (e2 :+: e3)) h e
assocRSumH = SumH . sumH (sumH L1 (R1 . SumH . L1)) (R1 . SumH . R1)

absurdLSumH :: (LiftIns NopI :+: e) h e' ~> e h e'
absurdLSumH = sumH (absurdNop . unliftIns) id

absurdRSumH :: (e :+: LiftIns NopI) h e' ~> e h e'
absurdRSumH = sumH id (absurdNop . unliftIns)

class SFunctor (e :: SigClass) where
    sfmap ::
        (HyperFunctor h, SFunctor e1, SFunctor e2) =>
        (forall x. SFunctor x => h ('ASigClass (x :+: e1)) ~> h ('ASigClass (x :+: e2))) ->
        e h ('ASigClass e1) ~> e h ('ASigClass e2)

    transHyper ::
        (forall x. SFunctor x => (h # x) ~> (h' # x)) ->
        (e h # e') ~> (e h' # e')

hysfmap ::
    (HyperFunctor h, SFunctor e1, SFunctor e2) =>
    (forall x. SFunctor x => (e1 h # x) ~> (e2 h # x)) ->
    (h # e1) ~> (h # e2)
hysfmap f = hyfmap $ shyfmap f . f
{-# INLINE hysfmap #-}

shyfmap ::
    (HyperFunctor h, SFunctor e, SFunctor e1, SFunctor e2) =>
    (forall x. SFunctor x => (e1 h # x) ~> (e2 h # x)) ->
    (e h # e1) ~> (e h # e2)
shyfmap f = sfmap (hysfmap $ SumH . sumH L1 (R1 . f))
{-# INLINE shyfmap #-}

instance SFunctor (LiftIns e) where
    sfmap _ = LiftIns . unliftIns
    transHyper _ = LiftIns . unliftIns
    {-# INLINE sfmap #-}
    {-# INLINE transHyper #-}

instance (SFunctor e1, SFunctor e2) => SFunctor (e1 :+: e2) where
    sfmap f = SumH . caseSum (L1 . sfmap f) (R1 . sfmap f) . unSumH
    {-# INLINE sfmap #-}

    transHyper f = SumH . caseSum (L1 . transHyper f) (R1 . transHyper f) . unSumH
    {-# INLINE transHyper #-}

data StateI s a where
    Put :: s -> StateI s ()
    Get :: StateI s s

data RunState s h e a where
    RunState :: h (LiftIns (StateI s) +# e) a -> RunState s h e (s, a)

instance SFunctor (RunState s) where
    sfmap f (RunState a) = RunState $ f a

interpretR ::
    forall e r f c.
    (Freer c f, SFunctor r, SFunctor e) =>
    (forall x. SFunctor x => (e (HyperFree f) # x :+: r) ~> (HyperFree f # x :+: r)) ->
    (HyperFree f # r :+: e) ~> (HyperFree f # r)
interpretR i =
    overHyperFree $
        interpretFreer $
            sumH
                (liftIns . SumH . sumH (L1 . sfmap undefined) undefined)
                undefined
  where
    int :: SFunctor x => (HyperFree f # (x :+: r) :+: e) ~> (HyperFree f # x :+: r)
    int = interpretR i

{-
interpretR ::
    forall e r f c.
    (Freer c f, SFunctor r, SFunctor e) =>
    (forall x. SFunctor x => (e (HyperFree f) # x :+: r) ~> (HyperFree f # x :+: r)) ->
    (HyperFree f # r :+: e) ~> (HyperFree f # r)
interpretR i =
    overHyperFree $
        interpretFreer $
            sumH
                (liftIns . sfmap int)
                ( unHyperFree
                    . absurdHyperFreeL
                    . i
                    . sfmap (hysfmap (SumH . sumH L1 (R1 . SumH . R1)) . int)
                )
  where
    int :: SFunctor x => (HyperFree f # x :+: (r :+: e)) ~> (HyperFree f # x :+: r)
    int = interpretR (hysfmap assocRSumH . i . shyfmap assocLSumH) . hysfmap assocLSumH
-}

absurdHyperFreeL :: (Freer c f, SFunctor e) => (HyperFree f # LiftIns NopI :+: e) ~> (HyperFree f # e)
absurdHyperFreeL = transformHyperFree $ sumH (absurdNop . unliftIns) (shyfmap absurdLSumH)

absurdHyperFreeR :: (Freer c f, SFunctor e) => (HyperFree f # e :+: LiftIns NopI) ~> (HyperFree f # e)
absurdHyperFreeR = transformHyperFree $ sumH (shyfmap absurdRSumH) (absurdNop . unliftIns)

absurdNop :: NopI a -> x
absurdNop = \case {}

transformHyperFree ::
    Freer c f =>
    ((e (HyperFree f) # e) ~> (e' (HyperFree f) # e')) ->
    (HyperFree f # e) ~> (HyperFree f # e')
transformHyperFree f = overHyperFree $ transformFreer f

{-
interpretL ::
    forall e r f c.
    (Freer c f, SFunctor r, SFunctor e) =>
    (forall x. SFunctor x => (e (HyperFree f) # r :+: x) ~> (HyperFree f # r :+: x)) ->
    (HyperFree f # e :+: r) ~> (HyperFree f # r)
interpretL i =
    overHyperFree $
        interpretFreer $
            sumH
                ( unHyperFree
                    . absurdHyperFreeR
                    . i
                    . sfmap (hysfmap (SumH . sumH L1 (R1 . SumH . L1)) . int)
                )
                (liftIns . sfmap int)
  where
    int :: SFunctor x => (HyperFree f # x :+: (e :+: r)) ~> (HyperFree f # x :+: r)
    int = undefined
-}
