module Control.Hefty.Trans.Final.Naked where

import Control.Free.Class (Freer, liftIns, retractF)
import Control.Hefty (HFunctor, Signature, hmap)
import Control.Hefty.Final (Noop)
import Control.Hefty.Trans.Final (
    HeftierFinalT (HeftierFinalT),
    InterpreterT (InterpreterT),
    interpretLower,
    interpreter,
 )
import Data.Hefty.Sum (type (+) (L, R))

newtype HeftierFinalTN (h :: Signature) f a = HeftierFinalN
    {unHeftierFinalTN :: forall g. InterpreterT h f g -> g a}

runHeftierFinalTN :: InterpreterT h f g -> HeftierFinalTN h f a -> g a
runHeftierFinalTN i (HeftierFinalN f) = f i

liftSigFinalTN :: HFunctor h => h (HeftierFinalTN h f) a -> HeftierFinalTN h f a
liftSigFinalTN e = HeftierFinalN \i -> interpreter i $ hmap (runHeftierFinalTN i) e

toHeftierFinalT :: HeftierFinalTN h f a -> HeftierFinalT Noop h f a
toHeftierFinalT (HeftierFinalN f) = HeftierFinalT f

fromHeftierFinalT :: HeftierFinalT Noop h f a -> HeftierFinalTN h f a
fromHeftierFinalT (HeftierFinalT f) = HeftierFinalN f

toHeftierFinalTF :: Freer c g => HeftierFinalTN (g + h) f a -> HeftierFinalT c h f a
toHeftierFinalTF (HeftierFinalN f) =
    HeftierFinalT \i -> f $ InterpreterT (interpretLower i) \case
        L m -> retractF m
        R e -> interpreter i e

fromHeftierFinalTF :: (Freer c g, HFunctor h) => HeftierFinalT c h f a -> HeftierFinalTN (g + h) f a
fromHeftierFinalTF (HeftierFinalT f) =
    HeftierFinalN \i ->
        interpreter i . L . f $
            InterpreterT
                (liftIns . interpretLower i)
                (liftIns . interpreter i . R . hmap (interpreter i . L))
