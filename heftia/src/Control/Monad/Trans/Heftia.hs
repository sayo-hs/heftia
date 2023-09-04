{-# LANGUAGE QuantifiedConstraints #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Monad.Trans.Heftia where

import Control.Effect.Class (Signature, type (~>))
import Control.Effect.Class.Machinery.HFunctor (HFunctor, (:+:) (Inl, Inr))
import Control.Heftia.Trans (TransHeftia, hoistHeftia, interpretHT, liftLower, liftSigT, translateT)
import Control.Monad.Cont (ContT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)

class
    (TransHeftia Monad h, forall sig. HFunctor sig => MonadTrans (h sig)) =>
    MonadTransHeftia h
    where
    interpretK ::
        (Monad m, HFunctor sig) =>
        (sig (ContT b m) ~> ContT b m) ->
        h sig m a ->
        ContT b m a
    interpretK = interpretTT
    {-# INLINE interpretK #-}

    reinterpretK ::
        (Monad m, HFunctor sig) =>
        (sig (ContT b (h sig m)) ~> ContT b (h sig m)) ->
        h sig m a ->
        ContT b (h sig m) a
    reinterpretK = reinterpretTT
    {-# INLINE reinterpretK #-}

    interpretTT ::
        (Monad m, MonadTrans t, Monad (t m), HFunctor sig) =>
        (sig (t m) ~> t m) ->
        h sig m a ->
        t m a
    interpretTT = interpretHT lift
    {-# INLINE interpretTT #-}

    reinterpretTT ::
        forall m t n sig a.
        (Monad m, MonadTrans t, Coercible n (h sig m), Monad (t n), Monad n, HFunctor sig) =>
        (sig (t n) ~> t n) ->
        h sig m a ->
        t n a
    reinterpretTT f = interpretTT f . hoistHeftia (coerce . liftLower @Monad @h @sig)
    {-# INLINE reinterpretTT #-}

mergeHeftia ::
    forall h m sig sig' a c.
    (HFunctor sig, HFunctor sig', TransHeftia c h, c m) =>
    h sig (h sig' m) a ->
    h (sig :+: sig') m a
mergeHeftia = interpretHT (translateT @c Inr) (liftSigT @c . Inl)

reinterpretTTViaFinal ::
    forall h m t n sig a.
    ( MonadTransHeftia h
    , Monad m
    , MonadTrans t
    , Coercible n (h sig m)
    , Monad (t n)
    , Monad n
    , HFunctor sig
    ) =>
    (sig (t n) ~> t n) ->
    h sig m a ->
    t n a
reinterpretTTViaFinal = interpretHT $ lift . coerce . liftLower @Monad @h @sig
{-# INLINE reinterpretTTViaFinal #-}

newtype ViaLiftLower (h :: Signature -> (Type -> Type) -> Type -> Type) sig m a = ViaLiftLower
    {runViaLiftLower :: h sig m a}
    deriving newtype (Functor, Applicative, Monad)
    deriving stock (Foldable, Traversable)

instance (TransHeftia Monad h, HFunctor sig) => MonadTrans (ViaLiftLower h sig) where
    lift = ViaLiftLower . liftLower
    {-# INLINE lift #-}
