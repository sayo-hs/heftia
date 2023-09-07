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
import Control.Monad.Trans.Heftia (MonadTransHeftia, ViaLiftLowerH (ViaLiftLowerH))

newtype HeftiaFinalT c h f a = HeftiaFinalT
    {unHeftiaFinalT :: HeftiaFinal c (h :+: LiftIns f) a}

data FinalTElaborator h f g = FinalTElaborator
    { elaborateFinalTLower :: f ~> g
    , elaborateFinalT :: h g ~> g
    }

runHeftiaFinalT :: c g => FinalTElaborator h f g -> HeftiaFinalT c h f a -> g a
runHeftiaFinalT FinalTElaborator{..} (HeftiaFinalT (HeftiaFinal h)) = h \case
    Inl e -> elaborateFinalT e
    Inr (LiftIns a) -> elaborateFinalTLower a

heftiaFinalT :: (forall g. c g => FinalTElaborator h f g -> g a) -> HeftiaFinalT c h f a
heftiaFinalT f = HeftiaFinalT $ HeftiaFinal \i -> f $ FinalTElaborator (i . Inr . LiftIns) (i . Inl)

liftSigFinalT :: HFunctor h => h (HeftiaFinalT c h f) a -> HeftiaFinalT c h f a
liftSigFinalT = HeftiaFinalT . liftSigFinal . Inl . hfmap unHeftiaFinalT
{-# INLINE liftSigFinalT #-}

liftLowerHFinal :: HFunctor h => f a -> HeftiaFinalT c h f a
liftLowerHFinal = HeftiaFinalT . liftSigFinal . Inr . LiftIns
{-# INLINE liftLowerHFinal #-}

weakenHeftiaFinalT :: (forall g. c' g => c g) => HeftiaFinalT c h f a -> HeftiaFinalT c' h f a
weakenHeftiaFinalT = HeftiaFinalT . weakenHeftiaFinal . unHeftiaFinalT
{-# INLINE weakenHeftiaFinalT #-}

hoistHeftiaFinal ::
    (f ~> g) ->
    HeftiaFinalT c h f ~> HeftiaFinalT c h g
hoistHeftiaFinal phi (HeftiaFinalT a) =
    HeftiaFinalT $ ($ a) $ transformHeftiaFinal \case
        Inl e -> Inl e
        Inr (LiftIns a') -> Inr $ LiftIns $ phi a'

deriving newtype instance Functor (HeftiaFinalT Functor h f)

deriving newtype instance Functor (HeftiaFinalT Applicative h f)
deriving newtype instance Applicative (HeftiaFinalT Applicative h f)

deriving newtype instance Functor (HeftiaFinalT Alternative h f)
deriving newtype instance Applicative (HeftiaFinalT Alternative h f)
deriving newtype instance Alternative (HeftiaFinalT Alternative h f)

deriving newtype instance Functor (HeftiaFinalT Monad h m)
deriving newtype instance Applicative (HeftiaFinalT Monad h m)
deriving newtype instance Monad (HeftiaFinalT Monad h m)

deriving newtype instance Functor (HeftiaFinalT MonadPlus h m)
deriving newtype instance Applicative (HeftiaFinalT MonadPlus h m)
deriving newtype instance Alternative (HeftiaFinalT MonadPlus h m)
deriving newtype instance Monad (HeftiaFinalT MonadPlus h m)
deriving newtype instance MonadPlus (HeftiaFinalT MonadPlus h m)

instance (forall h f. c f => c (HeftiaFinalT c h f)) => TransHeftia c (HeftiaFinalT c) where
    liftSigT = liftSigFinalT
    {-# INLINE liftSigT #-}

    translateT f (HeftiaFinalT a) =
        ($ a) $ runHeftiaFinal \case
            Inl e -> liftSigFinalT $ f e
            Inr (LiftIns a') -> liftLowerH a'

    liftLowerH = liftLowerHFinal
    {-# INLINE liftLowerH #-}

    runElaborateH i = runHeftiaFinalT $ FinalTElaborator id i
    {-# INLINE runElaborateH #-}

    hoistHeftia = hoistHeftiaFinal
    {-# INLINE hoistHeftia #-}

deriving via
    ViaLiftLowerH (HeftiaFinalT Monad) h
    instance
        HFunctor h => MonadTrans (HeftiaFinalT Monad h)

instance MonadTransHeftia (HeftiaFinalT Monad)

subsumeHeftiaFinal ::
    (c (HeftiaFinalT c h f), HFunctor h) =>
    HeftiaFinalT c h (HeftiaFinalT c h f) a ->
    HeftiaFinalT c h f a
subsumeHeftiaFinal (HeftiaFinalT (HeftiaFinal f)) =
    f \case
        Inl e -> liftSigFinalT e
        Inr (LiftIns e) -> e

dupHeftiaFinal :: HFunctor h => HeftiaFinalT c h f a -> HeftiaFinalT c h (HeftiaFinalT c h f) a
dupHeftiaFinal = hoistHeftiaFinal liftLowerHFinal
{-# INLINE dupHeftiaFinal #-}
