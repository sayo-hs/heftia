module Control.Hefty.Trans.Final.Naked where

import Control.Free.Class (Freer, liftIns, retractF)
import Control.Hefty (HFunctor, LiftIns, Signature, hmap)
import Control.Hefty.Final (Noop)
import Control.Hefty.Final.Naked (HeftierFinalN, nakeHeftierFinal, wearHeftierFinal)
import Control.Hefty.Trans.Final (
    HeftierFinalT (HeftierFinalT),
    InterpreterT (InterpreterT),
    heftierFinalT,
    interpretLower,
    interpreter,
    runHeftierFinalT,
    unHeftierFinalT,
 )
import Data.Hefty.Sum (type (+) (L, R))

newtype HeftierFinalTN (h :: Signature) f a = HeftierFinalTN
    {unHeftierFinalTN :: forall g. InterpreterT h f g -> g a}

runHeftierFinalTN :: InterpreterT h f g -> HeftierFinalTN h f a -> g a
runHeftierFinalTN i (HeftierFinalTN f) = f i

liftSigFinalTN :: HFunctor h => h (HeftierFinalTN h f) a -> HeftierFinalTN h f a
liftSigFinalTN e = HeftierFinalTN \i -> interpreter i $ hmap (runHeftierFinalTN i) e

wearHeftierFinalT :: HeftierFinalTN h f a -> HeftierFinalT Noop h f a
wearHeftierFinalT (HeftierFinalTN f) = heftierFinalT f

nakeHeftierFinalT :: HeftierFinalT Noop h f a -> HeftierFinalTN h f a
nakeHeftierFinalT m = HeftierFinalTN (`runHeftierFinalT` m)

wearHeftierFinalTF :: Freer c g => HeftierFinalTN (g + h) f a -> HeftierFinalT c h f a
wearHeftierFinalTF (HeftierFinalTN f) =
    heftierFinalT \i -> f $ InterpreterT (interpretLower i) \case
        L m -> retractF m
        R e -> interpreter i e

nakeHeftierFinalTF :: (Freer c g, HFunctor h) => HeftierFinalT c h f a -> HeftierFinalTN (g + h) f a
nakeHeftierFinalTF m =
    HeftierFinalTN \i ->
        interpreter i . L . (`runHeftierFinalT` m) $
            InterpreterT
                (liftIns . interpretLower i)
                (liftIns . interpreter i . R . hmap (interpreter i . L))

cisHeftierFinalN :: HeftierFinalTN h f a -> HeftierFinalN (h + LiftIns f) a
cisHeftierFinalN = nakeHeftierFinal . unHeftierFinalT . wearHeftierFinalT

transHeftierFinalN :: HeftierFinalN (h + LiftIns f) a -> HeftierFinalTN h f a
transHeftierFinalN = nakeHeftierFinalT . HeftierFinalT . wearHeftierFinal
