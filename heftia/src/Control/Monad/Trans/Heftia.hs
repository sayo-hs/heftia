{-# LANGUAGE QuantifiedConstraints #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

A type class to abstract away the encoding details of the Heftia monad transformers.
-}
module Control.Monad.Trans.Heftia where

import Control.Effect.Class (Signature, type (~>))
import Control.Effect.Class.Machinery.HFunctor (HFunctor, (:+:) (Inl, Inr))
import Control.Heftia.Trans (TransHeftia, elaborateHT, hoistHeftia, liftLowerHT, liftSigT, translateT)
import Control.Monad.Cont (ContT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)

-- | A type class to abstract away the encoding details of the Heftia monad transformers.
class
    (TransHeftia Monad h, forall sig. HFunctor sig => MonadTrans (h sig)) =>
    MonadTransHeftia h
    where
    elaborateMK ::
        (Monad m, HFunctor sig) =>
        (sig (ContT r m) ~> ContT r m) ->
        h sig m ~> ContT r m
    default elaborateMK ::
        (Monad m, HFunctor sig) =>
        (sig (ContT r m) ~> ContT r m) ->
        h sig m ~> ContT r m
    elaborateMK = elaborateMT
    {-# INLINE elaborateMK #-}

    reelaborateMK ::
        (Monad m, HFunctor sig) =>
        (sig (ContT r (h sig m)) ~> ContT r (h sig m)) ->
        h sig m ~> ContT r (h sig m)
    reelaborateMK = reelaborateMT
    {-# INLINE reelaborateMK #-}

    elaborateMT ::
        (Monad m, MonadTrans t, Monad (t m), HFunctor sig) =>
        (sig (t m) ~> t m) ->
        h sig m ~> t m
    elaborateMT = elaborateHT lift
    {-# INLINE elaborateMT #-}

    reelaborateMT ::
        forall m t n sig.
        (Monad m, MonadTrans t, Coercible n (h sig m), Monad (t n), Monad n, HFunctor sig) =>
        (sig (t n) ~> t n) ->
        h sig m ~> t n
    reelaborateMT f = elaborateMT f . hoistHeftia (coerce . liftLowerHT @Monad @h @sig)
    {-# INLINE reelaborateMT #-}

mergeHeftia ::
    forall h m sig sig' a c.
    (HFunctor sig, HFunctor sig', TransHeftia c h, c m) =>
    h sig (h sig' m) a ->
    h (sig :+: sig') m a
mergeHeftia = elaborateHT (translateT @c Inr) (liftSigT @c . Inl)

reinterpretHTTViaFinal ::
    forall h m t n sig.
    ( MonadTransHeftia h
    , Monad m
    , MonadTrans t
    , Coercible n (h sig m)
    , Monad (t n)
    , Monad n
    , HFunctor sig
    ) =>
    (sig (t n) ~> t n) ->
    h sig m ~> t n
reinterpretHTTViaFinal = elaborateHT $ lift . coerce . liftLowerHT @Monad @h @sig
{-# INLINE reinterpretHTTViaFinal #-}

newtype ViaLiftLowerH (h :: Signature -> (Type -> Type) -> Type -> Type) sig m a = ViaLiftLowerH
    {runViaLiftLowerH :: h sig m a}
    deriving newtype (Functor, Applicative, Monad)
    deriving stock (Foldable, Traversable)

instance (TransHeftia Monad h, HFunctor sig) => MonadTrans (ViaLiftLowerH h sig) where
    lift = ViaLiftLowerH . liftLowerHT
    {-# INLINE lift #-}
