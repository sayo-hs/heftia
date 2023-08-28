{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Heftier.Trans.Final where

import Control.Applicative (Alternative)
import Control.Heftier.Final (HeftierFinal (HeftierFinal), liftSigFinal, weakenHeftierFinal)
import Control.Hefty (HFunctor, LiftIns (LiftIns), hmap)
import Control.Monad (MonadPlus)
import Control.Natural (type (~>))
import Data.Constraint (Class)
import Data.Hefty.Sum (type (+) (L, R))

newtype HeftierFinalT c h f a = HeftierFinalT
    {unHeftierFinalT :: HeftierFinal c (h + LiftIns f) a}

data InterpreterT h f g = InterpreterT
    { interpretLower :: f ~> g
    , interpreter :: h g ~> g
    }

runHeftierFinalT :: c g => InterpreterT h f g -> HeftierFinalT c h f a -> g a
runHeftierFinalT InterpreterT{..} (HeftierFinalT (HeftierFinal f)) = f \case
    L e -> interpreter e
    R (LiftIns a) -> interpretLower a

heftierFinalT :: (forall g. c g => InterpreterT h f g -> g a) -> HeftierFinalT c h f a
heftierFinalT f = HeftierFinalT $ HeftierFinal \i -> f $ InterpreterT (i . R . LiftIns) (i . L)

liftSigFinalT :: HFunctor h => h (HeftierFinalT c h f) a -> HeftierFinalT c h f a
liftSigFinalT = HeftierFinalT . liftSigFinal . L . hmap unHeftierFinalT

weakenHeftierFinalT :: (forall g. c' g => c g) => HeftierFinalT c h f a -> HeftierFinalT c' h f a
weakenHeftierFinalT = HeftierFinalT . weakenHeftierFinal . unHeftierFinalT

deriving newtype instance
    (forall g. Class (Functor g) (c g)) =>
    Functor (HeftierFinalT c h f)

deriving newtype instance
    ( forall g. Class (Applicative g) (c g)
    , forall g. Class (Functor g) (c g)
    ) =>
    Applicative (HeftierFinalT c h f)

deriving newtype instance
    ( forall g. Class (Alternative g) (c g)
    , forall g. Class (Applicative g) (c g)
    , forall g. Class (Functor g) (c g)
    ) =>
    Alternative (HeftierFinalT c h f)

deriving newtype instance
    ( forall n. Class (Monad n) (c n)
    , forall n. Class (Applicative n) (c n)
    , forall n. Class (Functor n) (c n)
    ) =>
    Monad (HeftierFinalT c h m)

deriving newtype instance
    ( forall n. Class (MonadPlus n) (c n)
    , forall n. Class (Monad n) (c n)
    , forall n. Class (Alternative n) (c n)
    , forall n. Class (Applicative n) (c n)
    , forall n. Class (Functor n) (c n)
    ) =>
    MonadPlus (HeftierFinalT c h m)
