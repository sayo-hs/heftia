{-# LANGUAGE QuantifiedConstraints #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Heftia.Trans where

import Control.Effect.Class (type (~>))
import Control.Effect.Class.HFunctor (HFunctor, hfmap)
import Control.Heftia (Heftia, liftSig)
import Control.Monad.Identity (IdentityT (IdentityT), runIdentityT)

class (forall m. c m => Heftia c (h m)) => TransHeftia c h | h -> c where
    {-# MINIMAL liftLower, (hoistHeftia, interpretR | interpretT) #-}

    liftLower :: forall sig m a. (c m, HFunctor sig) => m a -> h m sig a

    -- | Translate an underlying monad.
    hoistHeftia :: (c m, c n, HFunctor sig) => (m ~> n) -> h m sig a -> h n sig a
    hoistHeftia phi = interpretT (liftLower . phi) (liftSig @c)
    {-# INLINE hoistHeftia #-}

    interpretR :: (c m, HFunctor sig) => (sig m ~> m) -> h m sig a -> m a
    default interpretR :: (c m, c (IdentityT m), HFunctor sig) => (sig m ~> m) -> h m sig a -> m a
    interpretR f = runIdentityT . interpretT IdentityT (IdentityT . f . hfmap runIdentityT)
    {-# INLINE interpretR #-}

    interpretT :: (c m, c n, HFunctor sig) => (m ~> n) -> (sig n ~> n) -> h m sig a -> n a
    interpretT phi i = interpretR i . hoistHeftia phi
    {-# INLINE interpretT #-}
