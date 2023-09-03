-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Heftia.Trans.Final.Naked where

import Control.Effect.Class (LiftIns, Signature)
import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap, (:+:) (Inl, Inr))
import Control.Freer (Freer, liftIns, retractF)
import Control.Heftia.Final (Noop)
import Control.Heftia.Final.Naked (HeftiaFinalN, nakeHeftiaFinal, wearHeftiaFinal)
import Control.Heftia.Trans.Final (
    HeftiaFinalT (HeftiaFinalT),
    InterpreterT (InterpreterT),
    heftiaFinalT,
    interpretLower,
    interpreter,
    runHeftiaFinalT,
    unHeftiaFinalT,
 )

newtype HeftiaFinalTN f (h :: Signature) a = HeftiaFinalTN
    {unHeftiaFinalTN :: forall g. InterpreterT h f g -> g a}

runHeftiaFinalTN :: InterpreterT h f g -> HeftiaFinalTN f h a -> g a
runHeftiaFinalTN i (HeftiaFinalTN f) = f i

liftSigFinalTN :: HFunctor h => h (HeftiaFinalTN f h) a -> HeftiaFinalTN f h a
liftSigFinalTN e = HeftiaFinalTN \i -> interpreter i $ hfmap (runHeftiaFinalTN i) e

wearHeftiaFinalT :: HeftiaFinalTN h f a -> HeftiaFinalT Noop h f a
wearHeftiaFinalT (HeftiaFinalTN f) = heftiaFinalT f

nakeHeftiaFinalT :: HeftiaFinalT Noop h f a -> HeftiaFinalTN h f a
nakeHeftiaFinalT m = HeftiaFinalTN (`runHeftiaFinalT` m)

wearHeftiaFinalTF :: Freer c g => HeftiaFinalTN f (g :+: h) a -> HeftiaFinalT c f h a
wearHeftiaFinalTF (HeftiaFinalTN f) =
    heftiaFinalT \i -> f $ InterpreterT (interpretLower i) \case
        Inl m -> retractF m
        Inr e -> interpreter i e

nakeHeftiaFinalTF :: (Freer c g, HFunctor h) => HeftiaFinalT c f h a -> HeftiaFinalTN f (g :+: h) a
nakeHeftiaFinalTF m =
    HeftiaFinalTN \i ->
        interpreter i . Inl . (`runHeftiaFinalT` m) $
            InterpreterT
                (liftIns . interpretLower i)
                (liftIns . interpreter i . Inr . hfmap (interpreter i . Inl))

cisHeftiaFinalN :: HeftiaFinalTN f h a -> HeftiaFinalN (h :+: LiftIns f) a
cisHeftiaFinalN = nakeHeftiaFinal . unHeftiaFinalT . wearHeftiaFinalT

transHeftiaFinalN :: HeftiaFinalN (h :+: LiftIns f) a -> HeftiaFinalTN f h a
transHeftiaFinalN = nakeHeftiaFinalT . HeftiaFinalT . wearHeftiaFinal
