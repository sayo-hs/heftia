{-# LANGUAGE QuantifiedConstraints #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Freer where

import Control.Applicative.Free (Ap, liftAp, runAp)
import Control.Effect.Class (type (~>))
import Data.Functor.Coyoneda (Coyoneda, hoistCoyoneda, liftCoyoneda, lowerCoyoneda)

class (forall ins. c (f ins)) => Freer c f | f -> c where
    {-# MINIMAL liftIns, (interpretF | retract, transformF) #-}

    -- | Lift a /instruction/ into a Freer monad.
    liftIns :: ins a -> f ins a

    interpretF :: c m => (ins ~> m) -> f ins a -> m a
    interpretF i = retract . transformF i
    {-# INLINE interpretF #-}

    retract :: c m => f m a -> m a
    retract = interpretF id
    {-# INLINE retract #-}

    -- | Translate /instruction/s embedded in a Freer monad.
    transformF ::
        (ins ~> ins') ->
        f ins a ->
        f ins' a
    transformF phi = interpretF $ liftIns . phi
    {-# INLINE transformF #-}

    reinterpretF :: (ins ~> f ins) -> f ins a -> f ins a
    reinterpretF = interpretF
    {-# INLINE reinterpretF #-}

instance Freer Functor Coyoneda where
    liftIns = liftCoyoneda
    interpretF i = lowerCoyoneda . hoistCoyoneda i
    {-# INLINE liftIns #-}
    {-# INLINE interpretF #-}

instance Freer Applicative Ap where
    liftIns = liftAp
    interpretF = runAp
    {-# INLINE liftIns #-}
    {-# INLINE interpretF #-}
