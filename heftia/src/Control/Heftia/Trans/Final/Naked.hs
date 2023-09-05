-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Heftia.Trans.Final.Naked where

import Control.Effect.Class (LiftIns, Signature)
import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap, (:+:) (Inl, Inr))
import Control.Freer (Freer, liftIns, retract)
import Control.Heftia.Final (Noop)
import Control.Heftia.Final.Naked (HeftiaFinalN, nakeHeftiaFinal, wearHeftiaFinal)
import Control.Heftia.Trans.Final (
    FinalTElaborator (FinalTElaborator),
    HeftiaFinalT (HeftiaFinalT),
    elaborateFinalT,
    elaborateFinalTLower,
    heftiaFinalT,
    runHeftiaFinalT,
    unHeftiaFinalT,
 )

newtype HeftiaFinalTN (h :: Signature) f a = HeftiaFinalTN
    {unHeftiaFinalTN :: forall g. FinalTElaborator h f g -> g a}

runHeftiaFinalTN :: FinalTElaborator h f g -> HeftiaFinalTN h f a -> g a
runHeftiaFinalTN i (HeftiaFinalTN f) = f i
{-# INLINE runHeftiaFinalTN #-}

liftSigFinalTN :: HFunctor h => h (HeftiaFinalTN h f) a -> HeftiaFinalTN h f a
liftSigFinalTN e = HeftiaFinalTN \i -> elaborateFinalT i $ hfmap (runHeftiaFinalTN i) e
{-# INLINE liftSigFinalTN #-}

wearHeftiaFinalT :: HeftiaFinalTN h f a -> HeftiaFinalT Noop h f a
wearHeftiaFinalT (HeftiaFinalTN f) = heftiaFinalT f
{-# INLINE wearHeftiaFinalT #-}

nakeHeftiaFinalT :: HeftiaFinalT Noop h f a -> HeftiaFinalTN h f a
nakeHeftiaFinalT m = HeftiaFinalTN (`runHeftiaFinalT` m)
{-# INLINE nakeHeftiaFinalT #-}

wearHeftiaFinalTF :: Freer c fr => HeftiaFinalTN (fr :+: h) f a -> HeftiaFinalT c h f a
wearHeftiaFinalTF (HeftiaFinalTN f) =
    heftiaFinalT \i -> f $ FinalTElaborator (elaborateFinalTLower i) \case
        Inl m -> retract m
        Inr e -> elaborateFinalT i e

nakeHeftiaFinalTF :: (Freer c fr, HFunctor h) => HeftiaFinalT c h f a -> HeftiaFinalTN (fr :+: h) f a
nakeHeftiaFinalTF m =
    HeftiaFinalTN \i ->
        elaborateFinalT i . Inl . (`runHeftiaFinalT` m) $
            FinalTElaborator
                (liftIns . elaborateFinalTLower i)
                (liftIns . elaborateFinalT i . Inr . hfmap (elaborateFinalT i . Inl))

cisHeftiaFinalN :: HeftiaFinalTN h f a -> HeftiaFinalN (h :+: LiftIns f) a
cisHeftiaFinalN = nakeHeftiaFinal . unHeftiaFinalT . wearHeftiaFinalT
{-# INLINE cisHeftiaFinalN #-}

transHeftiaFinalN :: HeftiaFinalN (h :+: LiftIns f) a -> HeftiaFinalTN h f a
transHeftiaFinalN = nakeHeftiaFinalT . HeftiaFinalT . wearHeftiaFinal
{-# INLINE transHeftiaFinalN #-}
