module Control.Monad.Heftier.Final where

import Control.Hefty (HFunctor, hmap)
import Control.Natural (type (~>))

newtype HeftierFinalM h a = HeftierFinalM {unHeftierFinal :: forall m. Monad m => (h m ~> m) -> m a}
    deriving (Functor)

instance Applicative (HeftierFinalM h) where
    pure x = HeftierFinalM \_ -> pure x
    HeftierFinalM f <*> HeftierFinalM g = HeftierFinalM \i -> f i <*> g i

instance Monad (HeftierFinalM h) where
    HeftierFinalM f >>= k = HeftierFinalM \i -> f i >>= runHeftierFinal i . k

runHeftierFinal :: Monad m => (h m ~> m) -> HeftierFinalM h a -> m a
runHeftierFinal i (HeftierFinalM f) = f i

liftSigFinal :: HFunctor h => h (HeftierFinalM h) a -> HeftierFinalM h a
liftSigFinal e = HeftierFinalM \i -> i $ hmap (runHeftierFinal i) e
