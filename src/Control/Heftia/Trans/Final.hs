{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Heftia.Trans.Final where

import Control.Applicative (Alternative)
import Control.Effect.Class (LiftIns (LiftIns))
import Control.Effect.Class.HFunctor (HFunctor, hmap)
import Control.Heftia.Final (HeftiaFinal (HeftiaFinal), liftSigFinal, weakenHeftiaFinal)
import Control.Monad (MonadPlus)
import Control.Natural (type (~>))
import Data.Constraint (Class)
import Data.Hefty.Sum (type (+) (L, R))

newtype HeftiaFinalT c h f a = HeftiaFinalT
    {unHeftiaFinalT :: HeftiaFinal c (h + LiftIns f) a}

data InterpreterT h f g = InterpreterT
    { interpretLower :: f ~> g
    , interpreter :: h g ~> g
    }

runHeftiaFinalT :: c g => InterpreterT h f g -> HeftiaFinalT c h f a -> g a
runHeftiaFinalT InterpreterT{..} (HeftiaFinalT (HeftiaFinal f)) = f \case
    L e -> interpreter e
    R (LiftIns a) -> interpretLower a

heftiaFinalT :: (forall g. c g => InterpreterT h f g -> g a) -> HeftiaFinalT c h f a
heftiaFinalT f = HeftiaFinalT $ HeftiaFinal \i -> f $ InterpreterT (i . R . LiftIns) (i . L)

liftSigFinalT :: HFunctor h => h (HeftiaFinalT c h f) a -> HeftiaFinalT c h f a
liftSigFinalT = HeftiaFinalT . liftSigFinal . L . hmap unHeftiaFinalT

weakenHeftiaFinalT :: (forall g. c' g => c g) => HeftiaFinalT c h f a -> HeftiaFinalT c' h f a
weakenHeftiaFinalT = HeftiaFinalT . weakenHeftiaFinal . unHeftiaFinalT

deriving newtype instance
    (forall g. Class (Functor g) (c g)) =>
    Functor (HeftiaFinalT c h f)

deriving newtype instance
    ( forall g. Class (Applicative g) (c g)
    , forall g. Class (Functor g) (c g)
    ) =>
    Applicative (HeftiaFinalT c h f)

deriving newtype instance
    ( forall g. Class (Alternative g) (c g)
    , forall g. Class (Applicative g) (c g)
    , forall g. Class (Functor g) (c g)
    ) =>
    Alternative (HeftiaFinalT c h f)

deriving newtype instance
    ( forall n. Class (Monad n) (c n)
    , forall n. Class (Applicative n) (c n)
    , forall n. Class (Functor n) (c n)
    ) =>
    Monad (HeftiaFinalT c h m)

deriving newtype instance
    ( forall n. Class (MonadPlus n) (c n)
    , forall n. Class (Monad n) (c n)
    , forall n. Class (Alternative n) (c n)
    , forall n. Class (Applicative n) (c n)
    , forall n. Class (Functor n) (c n)
    ) =>
    MonadPlus (HeftiaFinalT c h m)
