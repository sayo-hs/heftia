{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Heftia.Trans.Final where

import Control.Applicative (Alternative)
import Control.Effect.Class (LiftIns (LiftIns), type (~>))
import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap, (:+:) (Inl, Inr))
import Control.Heftia.Final (
    HeftiaFinal (HeftiaFinal),
    liftSigFinal,
    runHeftiaFinal,
    transformHeftiaFinal,
    weakenHeftiaFinal,
 )
import Control.Heftia.Trans (TransHeftia (..))
import Control.Monad (MonadPlus)
import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Heftia (MonadTransHeftia, ViaLiftLower (ViaLiftLower))

newtype HeftiaFinalT c h f a = HeftiaFinalT
    {unHeftiaFinalT :: HeftiaFinal c (h :+: LiftIns f) a}

data InterpreterT h f g = InterpreterT
    { interpretLower :: f ~> g
    , interpreter :: h g ~> g
    }

runHeftiaFinalT :: c g => InterpreterT h f g -> HeftiaFinalT c h f a -> g a
runHeftiaFinalT InterpreterT{..} (HeftiaFinalT (HeftiaFinal h)) = h \case
    Inl e -> interpreter e
    Inr (LiftIns a) -> interpretLower a

heftiaFinalT :: (forall g. c g => InterpreterT h f g -> g a) -> HeftiaFinalT c h f a
heftiaFinalT f = HeftiaFinalT $ HeftiaFinal \i -> f $ InterpreterT (i . Inr . LiftIns) (i . Inl)

liftSigFinalT :: HFunctor h => h (HeftiaFinalT c h f) a -> HeftiaFinalT c h f a
liftSigFinalT = HeftiaFinalT . liftSigFinal . Inl . hfmap unHeftiaFinalT
{-# INLINE liftSigFinalT #-}

liftLowerFinal :: HFunctor h => f a -> HeftiaFinalT c h f a
liftLowerFinal = HeftiaFinalT . liftSigFinal . Inr . LiftIns
{-# INLINE liftLowerFinal #-}

weakenHeftiaFinalT :: (forall g. c' g => c g) => HeftiaFinalT c h f a -> HeftiaFinalT c' h f a
weakenHeftiaFinalT = HeftiaFinalT . weakenHeftiaFinal . unHeftiaFinalT
{-# INLINE weakenHeftiaFinalT #-}

hoistHeftiaFinal ::
    (f ~> g) ->
    HeftiaFinalT c h f a ->
    HeftiaFinalT c h g a
hoistHeftiaFinal phi (HeftiaFinalT a) =
    HeftiaFinalT $ ($ a) $ transformHeftiaFinal \case
        Inl e -> Inl e
        Inr (LiftIns a') -> Inr $ LiftIns $ phi a'

deriving newtype instance
    (forall g. c g => Functor g, c (HeftiaFinal c (h :+: LiftIns f))) =>
    Functor (HeftiaFinalT c h f)

deriving newtype instance
    ( forall g. c g => Applicative g
    , c (HeftiaFinal c (h :+: LiftIns f))
    , c (HeftiaFinalT c h f)
    ) =>
    Applicative (HeftiaFinalT c h f)

deriving newtype instance
    ( forall g. c g => Alternative g
    , c (HeftiaFinal c (h :+: LiftIns f))
    , c (HeftiaFinalT c h f)
    ) =>
    Alternative (HeftiaFinalT c h f)

deriving newtype instance
    ( forall n. c n => Monad n
    , c (HeftiaFinal c (h :+: LiftIns m))
    , c (HeftiaFinalT c h m)
    ) =>
    Monad (HeftiaFinalT c h m)

deriving newtype instance
    ( forall n. c n => MonadPlus n
    , c (HeftiaFinal c (h :+: LiftIns m))
    , c (HeftiaFinalT c h m)
    ) =>
    MonadPlus (HeftiaFinalT c h m)

instance (forall h f. c f => c (HeftiaFinalT c h f)) => TransHeftia c (HeftiaFinalT c) where
    liftSigT = liftSigFinalT
    {-# INLINE liftSigT #-}

    translateT f (HeftiaFinalT a) =
        ($ a) $ runHeftiaFinal \case
            Inl e -> liftSigFinalT $ f e
            Inr (LiftIns a') -> liftLower a'

    liftLower = liftLowerFinal
    {-# INLINE liftLower #-}

    interpretR i = runHeftiaFinalT $ InterpreterT id i
    {-# INLINE interpretR #-}

    hoistHeftia = hoistHeftiaFinal
    {-# INLINE hoistHeftia #-}

deriving via
    ViaLiftLower (HeftiaFinalT Monad) h
    instance
        HFunctor h => MonadTrans (HeftiaFinalT Monad h)

instance MonadTransHeftia (HeftiaFinalT Monad)

joinHeftiaFinalT ::
    (c (HeftiaFinalT c h f), HFunctor h) =>
    HeftiaFinalT c h (HeftiaFinalT c h f) a ->
    HeftiaFinalT c h f a
joinHeftiaFinalT (HeftiaFinalT (HeftiaFinal f)) =
    f \case
        Inl e -> liftSigFinalT e
        Inr (LiftIns e) -> e

dupHeftiaFinalT :: HFunctor h => HeftiaFinalT c h f a -> HeftiaFinalT c h (HeftiaFinalT c h f) a
dupHeftiaFinalT = hoistHeftiaFinal liftLowerFinal
{-# INLINE dupHeftiaFinalT #-}
