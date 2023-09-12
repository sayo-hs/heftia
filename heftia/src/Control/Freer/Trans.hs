{-# LANGUAGE QuantifiedConstraints #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Freer.Trans where

import Control.Effect.Class (type (~>))
import Control.Monad.Identity (IdentityT (IdentityT), runIdentityT)

class (forall ins f. c f => c (fr ins f)) => TransFreer c fr | fr -> c where
    {-# MINIMAL liftInsT, liftLowerFT, (hoistFreer, runInterpretF | interpretFT) #-}

    -- | Lift a /instruction/ into a Freer monad transformer.
    liftInsT :: ins ~> fr ins f

    liftLowerFT :: forall ins f. c f => f ~> fr ins f

    -- | Translate /instruction/s embedded in a Freer monad transformer.
    transformT :: c f => (ins ~> ins') -> fr ins f ~> fr ins' f
    transformT f = interpretFT liftLowerFT (liftInsT . f)
    {-# INLINE transformT #-}

    -- | Translate an underlying monad.
    hoistFreer :: (c f, c g) => (f ~> g) -> fr ins f ~> fr ins g
    hoistFreer f = interpretFT (liftLowerFT . f) liftInsT
    {-# INLINE hoistFreer #-}

    interposeLowerT :: (c f, c g) => (f ~> fr ins g) -> fr ins f ~> fr ins g
    interposeLowerT f = interpretFT f liftInsT
    {-# INLINE interposeLowerT #-}

    runInterpretF :: c f => (ins ~> f) -> fr ins f a -> f a
    default runInterpretF :: (c f, c (IdentityT f)) => (ins ~> f) -> fr ins f a -> f a
    runInterpretF f = runIdentityT . interpretFT IdentityT (IdentityT . f)
    {-# INLINE runInterpretF #-}

    interpretFT :: (c f, c g) => (f ~> g) -> (ins ~> g) -> fr ins f ~> g
    interpretFT f i = runInterpretF i . hoistFreer f
    {-# INLINE interpretFT #-}

    reinterpretFT :: c f => (ins ~> fr ins f) -> fr ins f ~> fr ins f
    reinterpretFT = interpretFT liftLowerFT
    {-# INLINE reinterpretFT #-}
