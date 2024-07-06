{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

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

import Control.Applicative (Alternative)
import Control.Applicative.Free (Ap, liftAp, runAp)
import Control.Applicative.Free.Fast qualified as Fast
import Control.Effect (SendIns, sendIns, type (~>))
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Data.Effect (InsClass)
import Data.Functor.Coyoneda (Coyoneda, hoistCoyoneda, liftCoyoneda, lowerCoyoneda)
import Data.Kind (Type)
import Control.Effect.Key (SendInsBy, sendInsBy)

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

instance Freer Applicative Fast.Ap where
    liftIns = Fast.liftAp
    interpretFreer = Fast.runAp
    {-# INLINE liftIns #-}
    {-# INLINE interpretFreer #-}

newtype
    ViaFreer
        (fr :: InsClass -> Type -> Type)
        (e :: InsClass)
        (a :: Type) = ViaFreer
    {viaFreer :: fr e a}

deriving newtype instance Functor (fr e) => Functor (ViaFreer fr e)
deriving newtype instance Applicative (fr e) => Applicative (ViaFreer fr e)
deriving newtype instance Alternative (fr e) => Alternative (ViaFreer fr e)
deriving newtype instance Monad (fr e) => Monad (ViaFreer fr e)
deriving newtype instance MonadPlus (fr e) => MonadPlus (ViaFreer fr e)
deriving newtype instance (MonadBase b (fr e), Monad b) => MonadBase b (ViaFreer fr e)
deriving newtype instance MonadIO (fr e) => MonadIO (ViaFreer fr e)
deriving newtype instance MonadFail (fr e) => MonadFail (ViaFreer fr e)

deriving newtype instance Foldable (fr e) => Foldable (ViaFreer fr e)
deriving stock instance Traversable (fr e) => Traversable (ViaFreer fr e)
deriving newtype instance Eq (fr e a) => Eq (ViaFreer fr e a)
deriving newtype instance Ord (fr e a) => Ord (ViaFreer fr e a)
deriving newtype instance Read (fr e a) => Read (ViaFreer fr e a)
deriving newtype instance Show (fr e a) => Show (ViaFreer fr e a)

deriving newtype instance (Freer c fr, forall e. c (ViaFreer fr e)) => Freer c (ViaFreer fr)

instance (Freer c fr, InjectIns e e') => SendIns e (ViaFreer fr e') where
    sendIns = ViaFreer . liftIns . injectIns
    {-# INLINE sendIns #-}

class InjectIns e (e' :: InsClass) where
    injectIns :: e ~> e'

instance (Freer c fr, InjectInsBy key e e') => SendInsBy key e (ViaFreer fr e') where
    sendInsBy = ViaFreer . liftIns . injectInsBy @key
    {-# INLINE sendInsBy #-}

class InjectInsBy key e (e' :: InsClass) | key e' -> e where
    injectInsBy :: e ~> e'

overFreer :: (fr e a -> fr' e' b) -> ViaFreer fr e a -> ViaFreer fr' e' b
overFreer f = ViaFreer . f . viaFreer
{-# INLINE overFreer #-}

reencodeFreer :: (Freer c fr, Freer c' fr', c (fr' f)) => fr f ~> fr' f
reencodeFreer = interpretFreer liftIns
{-# INLINE reencodeFreer #-}
