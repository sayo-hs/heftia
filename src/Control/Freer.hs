{-# LANGUAGE QuantifiedConstraints #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Freer where

import Control.Applicative.Free (Ap, liftAp, runAp)
import Control.Effect.Class (type (~>))
import Data.Free.Union (weakenIns, type (<::))
import Data.Functor.Coyoneda (Coyoneda, hoistCoyoneda, liftCoyoneda, lowerCoyoneda)

class (forall ins. c (f ins)) => Freer c f | f -> c where
    {-# MINIMAL liftIns, (interpretFF | retractF, translateFF) #-}

    -- | Lift a /instruction/ into a Freer monad.
    liftIns :: ins a -> f ins a

    interpretFF :: c m => (ins ~> m) -> f ins a -> m a
    interpretFF i = retractF . translateFF i
    {-# INLINE interpretFF #-}

    retractF :: c m => f m a -> m a
    retractF = interpretFF id
    {-# INLINE retractF #-}

    -- | Translate /instruction/s embedded in a Freer monad.
    translateFF ::
        (ins ~> ins') ->
        f ins a ->
        f ins' a
    translateFF phi = interpretFF $ liftIns . phi
    {-# INLINE translateFF #-}

    reinterpretF :: (ins ~> f ins) -> f ins a -> f ins a
    reinterpretF = interpretFF
    {-# INLINE reinterpretF #-}

instance Freer Functor Coyoneda where
    liftIns = liftCoyoneda
    interpretFF i = lowerCoyoneda . hoistCoyoneda i
    {-# INLINE liftIns #-}
    {-# INLINE interpretFF #-}

instance Freer Applicative Ap where
    liftIns = liftAp
    interpretFF = runAp
    {-# INLINE liftIns #-}
    {-# INLINE interpretFF #-}

sendFF :: (i <:: j, Freer c f) => i a -> f j a
sendFF = liftIns . weakenIns
{-# INLINE sendFF #-}
