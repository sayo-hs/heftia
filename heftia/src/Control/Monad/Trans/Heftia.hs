-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Monad.Trans.Heftia where

import Control.Effect.Class (Signature, type (~>))
import Control.Effect.Class.HFunctor (HFunctor)
import Control.Heftia (liftSig, translateH)
import Control.Heftia.Trans (TransHeftia, hoistHeftia, interpretT, liftLower)
import Control.Monad.Cont (ContT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Coerce (Coercible, coerce)
import Data.Hefty.Sum (type (+) (L, R))
import Data.Kind (Type)

class TransHeftia Monad h => MonadTransHeftia h where
    interpretK ::
        (Monad m, HFunctor sig) =>
        (sig (ContT b m) ~> ContT b m) ->
        h m sig a ->
        ContT b m a
    interpretK = interpretTT
    {-# INLINE interpretK #-}

    reinterpretK ::
        (Monad m, HFunctor sig) =>
        (sig (ContT b (h m sig)) ~> ContT b (h m sig)) ->
        h m sig a ->
        ContT b (h m sig) a
    reinterpretK = reinterpretTT
    {-# INLINE reinterpretK #-}

    interpretTT ::
        (Monad m, MonadTrans t, Monad (t m), HFunctor sig) =>
        (sig (t m) ~> t m) ->
        h m sig a ->
        t m a
    interpretTT = interpretT lift
    {-# INLINE interpretTT #-}

    reinterpretTT ::
        forall m t n sig a.
        (Monad m, MonadTrans t, Coercible n (h m sig), Monad (t n), Monad n, HFunctor sig) =>
        (sig (t n) ~> t n) ->
        h m sig a ->
        t n a
    reinterpretTT f = interpretTT f . hoistHeftia (coerce . liftLower @Monad @h @sig)
    {-# INLINE reinterpretTT #-}

mergeHeftia ::
    forall h m sig sig' a c.
    (HFunctor sig, HFunctor sig', TransHeftia c h, c m) =>
    h (h m sig') sig a ->
    h m (sig + sig') a
mergeHeftia = interpretT (translateH @c R) (liftSig @c . L)

reinterpretTTViaFinal ::
    forall h m t n sig a.
    ( MonadTransHeftia h
    , Monad m
    , MonadTrans t
    , Coercible n (h m sig)
    , Monad (t n)
    , Monad n
    , HFunctor sig
    ) =>
    (sig (t n) ~> t n) ->
    h m sig a ->
    t n a
reinterpretTTViaFinal = interpretT $ lift . coerce . liftLower @Monad @h @sig
{-# INLINE reinterpretTTViaFinal #-}

newtype HeftiaT (h :: (Type -> Type) -> Signature -> Type -> Type) sig m a = HeftiaT
    {runHeftiaT :: h m sig a}
    deriving newtype (Functor, Applicative, Monad)
    deriving stock (Foldable, Traversable)

instance (MonadTransHeftia h, HFunctor sig) => MonadTrans (HeftiaT h sig) where
    lift = HeftiaT . liftLower
