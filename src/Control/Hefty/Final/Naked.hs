module Control.Hefty.Final.Naked where

import Control.Free.Class (Freer, liftIns, retractF)
import Control.Hefty (HFunctor, Signature, hmap)
import Control.Hefty.Final (HeftierFinal (HeftierFinal), Noop)
import Control.Natural (type (~>))
import Data.Hefty.Sum (type (+) (L, R))

newtype HeftierFinalN (h :: Signature) a = HeftierFinalN {unHeftierFinalN :: forall f. (h f ~> f) -> f a}

runHeftierFinalN :: (h f ~> f) -> HeftierFinalN h a -> f a
runHeftierFinalN i (HeftierFinalN f) = f i

liftSigFinalN :: HFunctor h => h (HeftierFinalN h) a -> HeftierFinalN h a
liftSigFinalN e = HeftierFinalN \i -> i $ hmap (runHeftierFinalN i) e

toHeftierFinal :: HeftierFinalN h a -> HeftierFinal Noop h a
toHeftierFinal (HeftierFinalN f) = HeftierFinal f

fromHeftierFinal :: HeftierFinal Noop h a -> HeftierFinalN h a
fromHeftierFinal (HeftierFinal f) = HeftierFinalN f

toHeftierFinalF :: Freer c f => HeftierFinalN (f + h) a -> HeftierFinal c h a
toHeftierFinalF (HeftierFinalN f) =
    HeftierFinal \i -> f \case
        L m -> retractF m
        R e -> i e

fromHeftierFinalF :: (Freer c f, HFunctor h) => HeftierFinal c h a -> HeftierFinalN (f + h) a
fromHeftierFinalF (HeftierFinal f) =
    HeftierFinalN \i -> i . L $ f $ liftIns . i . R . hmap (i . L)
