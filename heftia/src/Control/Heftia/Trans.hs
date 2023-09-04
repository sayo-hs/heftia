{-# LANGUAGE QuantifiedConstraints #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Heftia.Trans where

import Control.Effect.Class (type (~>))
import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap)
import Control.Monad.Identity (IdentityT (IdentityT), runIdentityT)

class (forall sig f. c f => c (h sig f)) => TransHeftia c h | h -> c where
    {-# MINIMAL liftSigT, translateT, liftLower, (hoistHeftia, interpretR | interpretHT) #-}

    -- | Lift a /signature/ into a Heftia monad transformer.
    liftSigT :: HFunctor sig => sig (h sig f) a -> h sig f a

    -- | Translate /signature/s embedded in a Heftia monad transformer.
    translateT ::
        (c f, HFunctor sig, HFunctor sig') =>
        (sig (h sig' f) ~> sig' (h sig' f)) ->
        h sig f a ->
        h sig' f a

    liftLower :: forall sig f a. (c f, HFunctor sig) => f a -> h sig f a

    -- | Translate an underlying monad.
    hoistHeftia :: (c f, c g, HFunctor sig) => (f ~> g) -> h sig f a -> h sig g a
    hoistHeftia phi = interpretHT (liftLower . phi) liftSigT
    {-# INLINE hoistHeftia #-}

    interpretR :: (c f, HFunctor sig) => (sig f ~> f) -> h sig f a -> f a
    default interpretR :: (c f, c (IdentityT f), HFunctor sig) => (sig f ~> f) -> h sig f a -> f a
    interpretR f = runIdentityT . interpretHT IdentityT (IdentityT . f . hfmap runIdentityT)
    {-# INLINE interpretR #-}

    interpretHT :: (c f, c g, HFunctor sig) => (f ~> g) -> (sig g ~> g) -> h sig f a -> g a
    interpretHT phi i = interpretR i . hoistHeftia phi
    {-# INLINE interpretHT #-}
