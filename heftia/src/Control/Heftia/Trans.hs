{-# LANGUAGE QuantifiedConstraints #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Heftia.Trans where

import Control.Effect.Class (type (~>))
import Control.Effect.Class.Machinery.HFunctor (HFunctor, hfmap)
import Control.Monad.Identity (IdentityT (IdentityT), runIdentityT)

class (forall sig n. c n => c (h n sig)) => TransHeftia c h | h -> c where
    {-# MINIMAL liftSigT, translateT, liftLower, (hoistHeftia, interpretR | interpretHT) #-}

    -- | Lift a /signature/ into a Heftia monad transformer.
    liftSigT :: HFunctor sig => sig (h m sig) a -> h m sig a

    -- | Translate /signature/s embedded in a Heftia monad transformer.
    translateT ::
        (c m, HFunctor sig, HFunctor sig') =>
        (sig (h m sig') ~> sig' (h m sig')) ->
        h m sig a ->
        h m sig' a

    liftLower :: forall sig m a. (c m, HFunctor sig) => m a -> h m sig a

    -- | Translate an underlying monad.
    hoistHeftia :: (c m, c n, HFunctor sig) => (m ~> n) -> h m sig a -> h n sig a
    hoistHeftia phi = interpretHT (liftLower . phi) liftSigT
    {-# INLINE hoistHeftia #-}

    interpretR :: (c m, HFunctor sig) => (sig m ~> m) -> h m sig a -> m a
    default interpretR :: (c m, c (IdentityT m), HFunctor sig) => (sig m ~> m) -> h m sig a -> m a
    interpretR f = runIdentityT . interpretHT IdentityT (IdentityT . f . hfmap runIdentityT)
    {-# INLINE interpretR #-}

    interpretHT :: (c m, c n, HFunctor sig) => (m ~> n) -> (sig n ~> n) -> h m sig a -> n a
    interpretHT phi i = interpretR i . hoistHeftia phi
    {-# INLINE interpretHT #-}
