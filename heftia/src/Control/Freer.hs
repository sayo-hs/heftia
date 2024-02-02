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

A type class to abstract away the encoding details of the Freer carriers.
-}
module Control.Freer where

import Control.Applicative.Free (Ap, liftAp, runAp)
import Control.Effect (type (~>))
import Data.Functor.Coyoneda (Coyoneda, hoistCoyoneda, liftCoyoneda, lowerCoyoneda)
import Data.Kind (Type)

type InsClass = Type -> Type

-- | A type class to abstract away the encoding details of the Freer carrier.
class (forall e. c (f e)) => Freer c f | f -> c where
    {-# MINIMAL liftIns, (interpretFreer | retractFreer, transformFreer) #-}

    -- | Lift a /instruction/ into a Freer carrier.
    liftIns :: e a -> f e a

    interpretFreer :: c m => (e ~> m) -> f e a -> m a
    interpretFreer i = retractFreer . transformFreer i
    {-# INLINE interpretFreer #-}

    retractFreer :: c m => f m a -> m a
    retractFreer = interpretFreer id
    {-# INLINE retractFreer #-}

    -- | Translate /instruction/s embedded in a Freer carrier.
    transformFreer ::
        (e ~> e') ->
        f e a ->
        f e' a
    transformFreer phi = interpretFreer $ liftIns . phi
    {-# INLINE transformFreer #-}

    reinterpretFreer :: (e ~> f e) -> f e a -> f e a
    reinterpretFreer = interpretFreer
    {-# INLINE reinterpretFreer #-}

instance Freer Functor Coyoneda where
    liftIns = liftCoyoneda
    interpretFreer i = lowerCoyoneda . hoistCoyoneda i
    {-# INLINE liftIns #-}
    {-# INLINE interpretFreer #-}

instance Freer Applicative Ap where
    liftIns = liftAp
    interpretFreer = runAp
    {-# INLINE liftIns #-}
    {-# INLINE interpretFreer #-}
