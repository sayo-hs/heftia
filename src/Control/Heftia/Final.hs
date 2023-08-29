{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Heftia.Final where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Effect.Class (Signature)
import Control.Effect.Class.HFunctor (HFunctor, hmap)
import Control.Monad (MonadPlus (mplus, mzero))
import Control.Natural (type (~>))
import Data.Constraint (Class, cls, (\\))

newtype HeftiaFinal c (h :: Signature) a = HeftiaFinal
    {unHeftiaFinal :: forall f. c f => (h f ~> f) -> f a}

runHeftiaFinal :: c f => (h f ~> f) -> HeftiaFinal c h a -> f a
runHeftiaFinal i (HeftiaFinal f) = f i

liftSigFinal :: HFunctor h => h (HeftiaFinal c h) a -> HeftiaFinal c h a
liftSigFinal e = HeftiaFinal \i -> i $ hmap (runHeftiaFinal i) e

weakenHeftiaFinal :: (forall f. c' f => c f) => HeftiaFinal c h a -> HeftiaFinal c' h a
weakenHeftiaFinal (HeftiaFinal f) = HeftiaFinal f

class Noop f
instance Noop f

instance (forall f. Class (Functor f) (c f)) => Functor (HeftiaFinal c h) where
    fmap f (HeftiaFinal g) =
        HeftiaFinal \(i :: h f ~> f) -> f <$> g i \\ cls @(Functor f) @(c f)

instance
    (forall f. Class (Applicative f) (c f), Functor (HeftiaFinal c h)) =>
    Applicative (HeftiaFinal c h)
    where
    pure x = HeftiaFinal \(_ :: h f ~> f) -> pure x \\ cls @(Applicative f) @(c f)

    HeftiaFinal f <*> HeftiaFinal g =
        HeftiaFinal \(i :: h f ~> f) -> f i <*> g i \\ cls @(Applicative f) @(c f)

instance
    (forall f. Class (Alternative f) (c f), Applicative (HeftiaFinal c h)) =>
    Alternative (HeftiaFinal c h)
    where
    empty = HeftiaFinal \(_ :: h f ~> f) -> empty \\ cls @(Alternative f) @(c f)

    HeftiaFinal f <|> HeftiaFinal g =
        HeftiaFinal \(i :: h f ~> f) -> f i <|> g i \\ cls @(Alternative f) @(c f)

instance
    (forall m. Class (Monad m) (c m), Applicative (HeftiaFinal c h)) =>
    Monad (HeftiaFinal c h)
    where
    HeftiaFinal f >>= k =
        HeftiaFinal \(i :: h m ~> m) ->
            f i >>= runHeftiaFinal i . k \\ cls @(Monad m) @(c m)

instance
    (forall m. Class (MonadPlus m) (c m), Alternative (HeftiaFinal c h), Monad (HeftiaFinal c h)) =>
    MonadPlus (HeftiaFinal c h)
    where
    mzero = HeftiaFinal \(_ :: h m ~> m) -> mzero \\ cls @(MonadPlus m) @(c m)

    HeftiaFinal f `mplus` HeftiaFinal g =
        HeftiaFinal \(i :: h m ~> m) -> f i `mplus` g i \\ cls @(MonadPlus m) @(c m)
