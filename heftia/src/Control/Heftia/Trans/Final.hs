{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Heftia.Trans.Final where

import Control.Applicative (Alternative)
import Control.Effect.Class (LiftIns (LiftIns), type (~>))
import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap, (:+:) (Inl, Inr))
import Control.Heftia.Final (HeftiaFinal (HeftiaFinal), liftSigFinal, runHeftiaFinal, transformHeftiaFinal, weakenHeftiaFinal)
import Control.Heftia.Trans (TransHeftia (..))
import Control.Monad (MonadPlus)
import Control.Monad.Trans.Heftia (MonadTransHeftia)

newtype HeftiaFinalT c f h a = HeftiaFinalT
    {unHeftiaFinalT :: HeftiaFinal c (h :+: LiftIns f) a}

data InterpreterT h f g = InterpreterT
    { interpretLower :: f ~> g
    , interpreter :: h g ~> g
    }

runHeftiaFinalT :: c g => InterpreterT h f g -> HeftiaFinalT c f h a -> g a
runHeftiaFinalT InterpreterT{..} (HeftiaFinalT (HeftiaFinal f)) = f \case
    Inl e -> interpreter e
    Inr (LiftIns a) -> interpretLower a

heftiaFinalT :: (forall g. c g => InterpreterT f h g -> g a) -> HeftiaFinalT c h f a
heftiaFinalT f = HeftiaFinalT $ HeftiaFinal \i -> f $ InterpreterT (i . Inr . LiftIns) (i . Inl)

liftSigFinalT :: HFunctor h => h (HeftiaFinalT c f h) a -> HeftiaFinalT c f h a
liftSigFinalT = HeftiaFinalT . liftSigFinal . Inl . hfmap unHeftiaFinalT

liftLowerFinal :: HFunctor h => f a -> HeftiaFinalT c f h a
liftLowerFinal = HeftiaFinalT . liftSigFinal . Inr . LiftIns

weakenHeftiaFinalT :: (forall g. c' g => c g) => HeftiaFinalT c f h a -> HeftiaFinalT c' f h a
weakenHeftiaFinalT = HeftiaFinalT . weakenHeftiaFinal . unHeftiaFinalT

hoistHeftiaFinal ::
    (f ~> g) ->
    HeftiaFinalT c f h a ->
    HeftiaFinalT c g h a
hoistHeftiaFinal phi (HeftiaFinalT a) =
    HeftiaFinalT $ ($ a) $ transformHeftiaFinal \case
        Inl e -> Inl e
        Inr (LiftIns a') -> Inr $ LiftIns $ phi a'

deriving newtype instance
    (forall g. c g => Functor g, c (HeftiaFinal c (h :+: LiftIns f))) =>
    Functor (HeftiaFinalT c f h)

deriving newtype instance
    ( forall g. c g => Applicative g
    , c (HeftiaFinal c (h :+: LiftIns f))
    , c (HeftiaFinalT c f h)
    ) =>
    Applicative (HeftiaFinalT c f h)

deriving newtype instance
    ( forall g. c g => Alternative g
    , c (HeftiaFinal c (h :+: LiftIns f))
    , c (HeftiaFinalT c f h)
    ) =>
    Alternative (HeftiaFinalT c f h)

deriving newtype instance
    ( forall n. c n => Monad n
    , c (HeftiaFinal c (h :+: LiftIns m))
    , c (HeftiaFinalT c m h)
    ) =>
    Monad (HeftiaFinalT c m h)

deriving newtype instance
    ( forall n. c n => MonadPlus n
    , c (HeftiaFinal c (h :+: LiftIns m))
    , c (HeftiaFinalT c m h)
    ) =>
    MonadPlus (HeftiaFinalT c m h)

instance (forall n sig. c n => c (HeftiaFinalT c n sig)) => TransHeftia c (HeftiaFinalT c) where
    liftSigT = liftSigFinalT

    translateT f (HeftiaFinalT a) =
        ($ a) $ runHeftiaFinal \case
            Inl e -> liftSigFinalT $ f e
            Inr (LiftIns a') -> liftLower a'

    liftLower = liftLowerFinal

    interpretR i = runHeftiaFinalT $ InterpreterT id i

    hoistHeftia = hoistHeftiaFinal

instance MonadTransHeftia (HeftiaFinalT Monad)

joinHeftiaFinalT ::
    (c (HeftiaFinalT c f h), HFunctor h) =>
    HeftiaFinalT c (HeftiaFinalT c f h) h a ->
    HeftiaFinalT c f h a
joinHeftiaFinalT (HeftiaFinalT (HeftiaFinal f)) =
    f \case
        Inl e -> liftSigFinalT e
        Inr (LiftIns e) -> e

dupHeftiaFinalT :: HFunctor h => HeftiaFinalT c f h a -> HeftiaFinalT c (HeftiaFinalT c f h) h a
dupHeftiaFinalT = hoistHeftiaFinal liftLowerFinal

{-
interpretT ::
    forall c h u e es t a.
    ( Heftia c h
    , Union u
    , HFunctor (u es)
    , HFunctor (u (e : es))
    , HFunctor e
    , c (HeftiaEffects h u (e : es))
    , c (t (HeftiaEffects h u (e : es)))
    , forall n sig. c n => c (HeftiaFinalT c n sig)
    , MonadTrans t
    , Monad (h (u (e ': es)))
    ) =>
    (e (t (HeftiaEffects h u es)) ~> t (HeftiaEffects h u es)) ->
    HeftiaEffects h u (e ': es) a ->
    t (HeftiaEffects h u es) a
interpretT i a = undefined
  where
    a'' :: t (HeftiaEffects h u (e ': es)) a
    a'' = undefined

    a' :: HeftiaFinalT c (t (HeftiaEffects h u es)) (u (e ': es)) a
    a' = liftLowerFinal undefined
-}
