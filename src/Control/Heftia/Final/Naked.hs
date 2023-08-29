module Control.Heftia.Final.Naked where

import Control.Effect.Class (Signature)
import Control.Effect.Class.HFunctor (HFunctor, hmap)
import Control.Freer (Freer, liftIns, retractF)
import Control.Heftia.Final (HeftiaFinal (HeftiaFinal), Noop)
import Control.Natural (type (~>))
import Data.Hefty.Sum (type (+) (L, R))

newtype HeftiaFinalN (h :: Signature) a = HeftiaFinalN {unHeftiaFinalN :: forall f. (h f ~> f) -> f a}

runHeftiaFinalN :: (h f ~> f) -> HeftiaFinalN h a -> f a
runHeftiaFinalN i (HeftiaFinalN f) = f i

liftSigFinalN :: HFunctor h => h (HeftiaFinalN h) a -> HeftiaFinalN h a
liftSigFinalN e = HeftiaFinalN \i -> i $ hmap (runHeftiaFinalN i) e

wearHeftiaFinal :: HeftiaFinalN h a -> HeftiaFinal Noop h a
wearHeftiaFinal (HeftiaFinalN f) = HeftiaFinal f

nakeHeftiaFinal :: HeftiaFinal Noop h a -> HeftiaFinalN h a
nakeHeftiaFinal (HeftiaFinal f) = HeftiaFinalN f

wearHeftiaFinalF :: Freer c f => HeftiaFinalN (f + h) a -> HeftiaFinal c h a
wearHeftiaFinalF (HeftiaFinalN f) =
    HeftiaFinal \i -> f \case
        L m -> retractF m
        R e -> i e

nakeHeftiaFinalF :: (Freer c f, HFunctor h) => HeftiaFinal c h a -> HeftiaFinalN (f + h) a
nakeHeftiaFinalF (HeftiaFinal f) =
    HeftiaFinalN \i -> i . L $ f $ liftIns . i . R . hmap (i . L)
