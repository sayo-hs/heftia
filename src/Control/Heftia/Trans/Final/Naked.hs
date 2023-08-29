-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Heftia.Trans.Final.Naked where

import Control.Effect.Class (LiftIns, Signature)
import Control.Effect.Class.HFunctor (HFunctor, hfmap)
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
import Data.Hefty.Sum (type (+) (L, R))

newtype HeftiaFinalTN (h :: Signature) f a = HeftiaFinalTN
    {unHeftiaFinalTN :: forall g. InterpreterT h f g -> g a}

runHeftiaFinalTN :: InterpreterT h f g -> HeftiaFinalTN h f a -> g a
runHeftiaFinalTN i (HeftiaFinalTN f) = f i

liftSigFinalTN :: HFunctor h => h (HeftiaFinalTN h f) a -> HeftiaFinalTN h f a
liftSigFinalTN e = HeftiaFinalTN \i -> interpreter i $ hfmap (runHeftiaFinalTN i) e

wearHeftiaFinalT :: HeftiaFinalTN h f a -> HeftiaFinalT Noop h f a
wearHeftiaFinalT (HeftiaFinalTN f) = heftiaFinalT f

nakeHeftiaFinalT :: HeftiaFinalT Noop h f a -> HeftiaFinalTN h f a
nakeHeftiaFinalT m = HeftiaFinalTN (`runHeftiaFinalT` m)

wearHeftiaFinalTF :: Freer c g => HeftiaFinalTN (g + h) f a -> HeftiaFinalT c h f a
wearHeftiaFinalTF (HeftiaFinalTN f) =
    heftiaFinalT \i -> f $ InterpreterT (interpretLower i) \case
        L m -> retractF m
        R e -> interpreter i e

nakeHeftiaFinalTF :: (Freer c g, HFunctor h) => HeftiaFinalT c h f a -> HeftiaFinalTN (g + h) f a
nakeHeftiaFinalTF m =
    HeftiaFinalTN \i ->
        interpreter i . L . (`runHeftiaFinalT` m) $
            InterpreterT
                (liftIns . interpretLower i)
                (liftIns . interpreter i . R . hfmap (interpreter i . L))

cisHeftiaFinalN :: HeftiaFinalTN h f a -> HeftiaFinalN (h + LiftIns f) a
cisHeftiaFinalN = nakeHeftiaFinal . unHeftiaFinalT . wearHeftiaFinalT

transHeftiaFinalN :: HeftiaFinalN (h + LiftIns f) a -> HeftiaFinalTN h f a
transHeftiaFinalN = nakeHeftiaFinalT . HeftiaFinalT . wearHeftiaFinal
