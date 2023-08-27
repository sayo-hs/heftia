{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Hefty.Final where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Hefty (HFunctor, Signature, hmap)
import Control.Monad (MonadPlus (mplus, mzero))
import Control.Natural (type (~>))
import Data.Constraint (Class, cls, (\\))

newtype HeftierFinal c (h :: Signature) a = HeftierFinal
    {unHeftierFinal :: forall f. c f => (h f ~> f) -> f a}

runHeftierFinal :: c f => (h f ~> f) -> HeftierFinal c h a -> f a
runHeftierFinal i (HeftierFinal f) = f i

liftSigFinal :: HFunctor h => h (HeftierFinal c h) a -> HeftierFinal c h a
liftSigFinal e = HeftierFinal \i -> i $ hmap (runHeftierFinal i) e

weakenHeftierFinal :: (forall f. c' f => c f) => HeftierFinal c h a -> HeftierFinal c' h a
weakenHeftierFinal (HeftierFinal f) = HeftierFinal f

class Noop f
instance Noop f

deriving stock instance Functor (HeftierFinal Applicative h)
deriving stock instance Functor (HeftierFinal Alternative h)
deriving stock instance Functor (HeftierFinal Monad h)
deriving stock instance Functor (HeftierFinal MonadPlus h)

instance (forall f. Class (Functor f) (c f)) => Functor (HeftierFinal c h) where
    fmap f (HeftierFinal g) =
        HeftierFinal \(i :: h f ~> f) -> f <$> g i \\ cls @(Functor f) @(c f)

instance
    (forall f. Class (Applicative f) (c f), Functor (HeftierFinal c h)) =>
    Applicative (HeftierFinal c h)
    where
    pure x = HeftierFinal \(_ :: h f ~> f) -> pure x \\ cls @(Applicative f) @(c f)

    HeftierFinal f <*> HeftierFinal g =
        HeftierFinal \(i :: h f ~> f) -> f i <*> g i \\ cls @(Applicative f) @(c f)

instance
    (forall f. Class (Alternative f) (c f), Applicative (HeftierFinal c h)) =>
    Alternative (HeftierFinal c h)
    where
    empty = HeftierFinal \(_ :: h f ~> f) -> empty \\ cls @(Alternative f) @(c f)

    HeftierFinal f <|> HeftierFinal g =
        HeftierFinal \(i :: h f ~> f) -> f i <|> g i \\ cls @(Alternative f) @(c f)

instance
    (forall m. Class (Monad m) (c m), Applicative (HeftierFinal c h)) =>
    Monad (HeftierFinal c h)
    where
    HeftierFinal f >>= k =
        HeftierFinal \(i :: h m ~> m) ->
            f i >>= runHeftierFinal i . k \\ cls @(Monad m) @(c m)

instance
    (forall m. Class (MonadPlus m) (c m), Alternative (HeftierFinal c h), Monad (HeftierFinal c h)) =>
    MonadPlus (HeftierFinal c h)
    where
    mzero = HeftierFinal \(_ :: h m ~> m) -> mzero \\ cls @(MonadPlus m) @(c m)

    HeftierFinal f `mplus` HeftierFinal g =
        HeftierFinal \(i :: h m ~> m) -> f i `mplus` g i \\ cls @(MonadPlus m) @(c m)
