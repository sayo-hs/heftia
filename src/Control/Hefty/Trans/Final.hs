{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Hefty.Trans.Final where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Hefty (HFunctor, LiftIns (LiftIns), hmap)
import Control.Hefty.Final (HeftierFinal (HeftierFinal))
import Control.Monad (MonadPlus, mplus, mzero)
import Control.Natural (type (~>))
import Data.Constraint (Class, cls, (\\))
import Data.Hefty.Sum (type (+) (L, R))

newtype HeftierFinalT c h f a = HeftierFinalT
    {unHeftierFinalT :: forall g. c g => InterpreterT h f g -> g a}

data InterpreterT h f g = InterpreterT
    { interpretLower :: f ~> g
    , interpreter :: h g ~> g
    }

runHeftierFinalT :: c g => InterpreterT h f g -> HeftierFinalT c h f a -> g a
runHeftierFinalT i (HeftierFinalT f) = f i

liftSigFinalT :: HFunctor h => h (HeftierFinalT c h f) a -> HeftierFinalT c h f a
liftSigFinalT e = HeftierFinalT \i -> interpreter i $ hmap (runHeftierFinalT i) e

weakenHeftierFinal :: (forall g. c' g => c g) => HeftierFinalT c h f a -> HeftierFinalT c' h f a
weakenHeftierFinal (HeftierFinalT f) = HeftierFinalT f

instance (forall g. Class (Functor g) (c g)) => Functor (HeftierFinalT c h f) where
    fmap f (HeftierFinalT g) =
        HeftierFinalT \(i :: InterpreterT h f g) -> f <$> g i \\ cls @(Functor g) @(c g)

instance
    (forall g. Class (Applicative g) (c g), Functor (HeftierFinalT c h f)) =>
    Applicative (HeftierFinalT c h f)
    where
    pure x = HeftierFinalT \(_ :: InterpreterT h f g) -> pure x \\ cls @(Applicative g) @(c g)

    HeftierFinalT f <*> HeftierFinalT g =
        HeftierFinalT \(i :: InterpreterT h f g) -> f i <*> g i \\ cls @(Applicative g) @(c g)

instance
    (forall g. Class (Alternative g) (c g), Applicative (HeftierFinalT c h f)) =>
    Alternative (HeftierFinalT c h f)
    where
    empty = HeftierFinalT \(_ :: InterpreterT h f g) -> empty \\ cls @(Alternative g) @(c g)

    HeftierFinalT f <|> HeftierFinalT g =
        HeftierFinalT \(i :: InterpreterT h f g) -> f i <|> g i \\ cls @(Alternative g) @(c g)

instance
    (forall n. Class (Monad n) (c n), Applicative (HeftierFinalT c h m)) =>
    Monad (HeftierFinalT c h m)
    where
    HeftierFinalT f >>= k =
        HeftierFinalT \(i :: InterpreterT h m n) ->
            f i >>= runHeftierFinalT i . k \\ cls @(Monad n) @(c n)

instance
    (forall m. Class (MonadPlus m) (c m), Alternative (HeftierFinalT c h f), Monad (HeftierFinalT c h f)) =>
    MonadPlus (HeftierFinalT c h f)
    where
    mzero = HeftierFinalT \(_ :: InterpreterT h m n) -> mzero \\ cls @(MonadPlus n) @(c n)

    HeftierFinalT f `mplus` HeftierFinalT g =
        HeftierFinalT \(i :: InterpreterT h m n) -> f i `mplus` g i \\ cls @(MonadPlus n) @(c n)

cisHeftierFinal :: HeftierFinalT c h f a -> HeftierFinal c (h + LiftIns f) a
cisHeftierFinal (HeftierFinalT f) =
    HeftierFinal \i -> f $ InterpreterT (i . R . LiftIns) (i . L)

transHeftierFinal :: HeftierFinal c (h + LiftIns f) a -> HeftierFinalT c h f a
transHeftierFinal (HeftierFinal f) =
    HeftierFinalT \InterpreterT{..} -> f \case
        L e -> interpreter e
        R (LiftIns a) -> interpretLower a
