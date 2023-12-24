{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Hefty.SFunctor where

import Control.Effect.Class (LiftIns (LiftIns), NopS, unliftIns, type (~>))
import Control.Effect.Class.Machinery.HFunctor (HFunctor, caseH, hfmap, (:+:) (Inl, Inr))
import Control.Freer (Freer, interpretFreer, liftIns, transformFreer)
import Control.Hefty (Hefty (Hefty), SigClass, overHefty, unHefty)
import Data.Kind (Type)

class HyperFunctor (h :: SigClass -> Type -> Type) where
    hyfmap :: (e (h e) ~> e' (h e')) -> h e ~> h e'

instance Freer c f => HyperFunctor (Hefty f) where
    hyfmap f = Hefty . transformFreer f . unHefty
    {-# INLINE hyfmap #-}

class SFunctor (e :: SigClass) where
    sfmap ::
        (HyperFunctor h, SFunctor e1, SFunctor e2) =>
        (forall ex. SFunctor ex => h (ex :+: e1) ~> h (ex :+: e2)) ->
        e (h e1) ~> e (h e2)

interpretRecRWithSFunctor ::
    forall r l f c.
    (Freer c f, SFunctor l, SFunctor r) =>
    (forall ex. SFunctor ex => r (Hefty f (ex :+: l)) ~> Hefty f (ex :+: l)) ->
    Hefty f (l :+: r) ~> Hefty f l
interpretRecRWithSFunctor i =
    overHefty $
        interpretFreer $
            caseH
                (liftIns . sfmap int)
                ( unHefty
                    . absurdHyperFunctorL
                    . i
                    . sfmap (hyshfmap (caseH Inl (Inr . Inr)) . int)
                )
  where
    int :: SFunctor ex' => Hefty f (ex' :+: (l :+: r)) ~> Hefty f (ex' :+: l)
    int =
        interpretRecRWithSFunctor (hyshfmap assocRSumH . i . shysfmap assocLSumH)
            . hyshfmap assocLSumH

hyshfmap ::
    forall e1 e2 h.
    ( HyperFunctor h
    , SFunctor e1
    , SFunctor e2
    ) =>
    (forall ex. e1 (h ex) ~> e2 (h ex)) ->
    h e1 ~> h e2
hyshfmap f = hyfmap $ shysfmap f . f
{-# INLINE hyshfmap #-}

shysfmap ::
    forall e e1 e2 h.
    ( HyperFunctor h
    , SFunctor e
    , SFunctor e1
    , SFunctor e2
    ) =>
    (forall ex. e1 (h ex) ~> e2 (h ex)) ->
    e (h e1) ~> e (h e2)
shysfmap f = sfmap $ hyshfmap $ caseH Inl (Inr . f)
{-# INLINE shysfmap #-}

hysfmap ::
    forall e e' h.
    (HyperFunctor h, SFunctor e, SFunctor e') =>
    (forall f. e f ~> e' f) ->
    h e ~> h e'
hysfmap f = hyfmap $ f . sfmap (rightHyperFunctor f)
{-# INLINE hysfmap #-}

rightHyperFunctor ::
    forall l r r' h.
    (HyperFunctor h, SFunctor l, SFunctor r, SFunctor r') =>
    (forall ex. r (h (ex :+: r')) ~> r' (h (ex :+: r'))) ->
    h (l :+: r) ~> h (l :+: r')
rightHyperFunctor f =
    hyfmap $ caseH (Inl . sfmapR f) (Inr . f . sfmapR f)

sfmapR ::
    forall e l r r' h.
    (HyperFunctor h, SFunctor e, SFunctor l, SFunctor r, SFunctor r') =>
    (forall ex. r (h (ex :+: r')) ~> r' (h (ex :+: r'))) ->
    e (h (l :+: r)) ~> e (h (l :+: r'))
sfmapR f =
    sfmap $
        assocHyperFunctorR
            . rightHyperFunctor f
            . assocHyperFunctorL

assocHyperFunctorL ::
    forall e1 e2 e3 h.
    (HyperFunctor h, SFunctor e1, SFunctor e2, SFunctor e3) =>
    h (e1 :+: (e2 :+: e3)) ~> h ((e1 :+: e2) :+: e3)
assocHyperFunctorL = hyshfmap assocLSumH

assocHyperFunctorR ::
    forall e1 e2 e3 h.
    (HyperFunctor h, SFunctor e1, SFunctor e2, SFunctor e3) =>
    h ((e1 :+: e2) :+: e3) ~> h (e1 :+: (e2 :+: e3))
assocHyperFunctorR = hyshfmap assocRSumH

instance SFunctor (LiftIns e) where
    sfmap _ = LiftIns . unliftIns
    {-# INLINE sfmap #-}

instance
    (SFunctor e1, SFunctor e2) =>
    SFunctor (e1 :+: e2)
    where
    sfmap f = caseH (Inl . sfmap f) (Inr . sfmap f)
    {-# INLINE sfmap #-}

newtype ViaHFunctor (e :: SigClass) f a = ViaHFunctor {unViaHFunctor :: e f a}
    deriving (HFunctor)

instance HFunctor e => SFunctor (ViaHFunctor e) where
    sfmap f = hfmap $ absurdHyperFunctorL . f . weakenHyperFunctorL
    {-# INLINE sfmap #-}

absurdHyperFunctorL ::
    forall e h.
    (HyperFunctor h, SFunctor e) =>
    h (NopS :+: e) ~> h e
absurdHyperFunctorL = hysfmap $ caseH (\case {}) id
{-# INLINE absurdHyperFunctorL #-}

weakenHyperFunctorL ::
    forall l r h.
    (HyperFunctor h, SFunctor l, SFunctor r) =>
    h r ~> h (l :+: r)
weakenHyperFunctorL = hysfmap Inr
{-# INLINE weakenHyperFunctorL #-}

assocLSumH :: (e1 :+: (e2 :+: e3)) f ~> ((e1 :+: e2) :+: e3) f
assocLSumH = caseH (Inl . Inl) (caseH (Inl . Inr) Inr)

assocRSumH :: ((e1 :+: e2) :+: e3) f ~> (e1 :+: (e2 :+: e3)) f
assocRSumH = caseH (caseH Inl (Inr . Inl)) (Inr . Inr)

data ((e :: SigClass) +# (f :: Type -> Type)) (a :: Type) where
    HyperFunctorPlusT :: h (e :+: r) a -> (e +# h r) a

deriving stock instance Functor (h (e :+: r)) => Functor (e +# h r)
deriving stock instance Foldable (h (e :+: r)) => Foldable (e +# h r)
deriving stock instance Traversable (h (e :+: r)) => Traversable (e +# h r)

instance SFunctor e => SFunctor ((+#) e) where
    sfmap f (HyperFunctorPlusT a) = HyperFunctorPlusT $ f a
    {-# INLINE sfmap #-}
