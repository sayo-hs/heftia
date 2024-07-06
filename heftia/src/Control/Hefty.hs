{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Hefty where

import Control.Applicative (Alternative)
import Control.Effect (SendIns (..), SendSig (..), type (~>))
import Control.Freer (Freer (liftIns), InjectIns, injectIns, InjectInsBy, injectInsBy)
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase)
import Control.Monad.IO.Class (MonadIO)
import Data.Effect (InsClass, SigClass)
import Data.Kind (Type)
import Control.Effect.Key (SendInsBy, sendInsBy, SendSigBy, sendSigBy)

newtype
    Hefty
        (f :: InsClass -> Type -> Type)
        (e :: SigClass)
        (a :: Type) = Hefty
    {unHefty :: f (e (Hefty f e)) a}

deriving newtype instance Functor (f (e (Hefty f e))) => Functor (Hefty f e)
deriving newtype instance Applicative (f (e (Hefty f e))) => Applicative (Hefty f e)
deriving newtype instance Alternative (f (e (Hefty f e))) => Alternative (Hefty f e)
deriving newtype instance Monad (f (e (Hefty f e))) => Monad (Hefty f e)
deriving newtype instance MonadPlus (f (e (Hefty f e))) => MonadPlus (Hefty f e)
deriving newtype instance (MonadBase b (f (e (Hefty f e))), Monad b) => MonadBase b (Hefty f e)
deriving newtype instance MonadIO (f (e (Hefty f e))) => MonadIO (Hefty f e)
deriving newtype instance MonadFail (f (e (Hefty f e))) => MonadFail (Hefty f e)

deriving newtype instance Foldable (f (e (Hefty f e))) => Foldable (Hefty f e)
deriving stock instance Traversable (f (e (Hefty f e))) => Traversable (Hefty f e)
deriving newtype instance Eq (f (e (Hefty f e)) a) => Eq (Hefty f e a)
deriving newtype instance Ord (f (e (Hefty f e)) a) => Ord (Hefty f e a)
deriving newtype instance Read (f (e (Hefty f e)) a) => Read (Hefty f e a)
deriving newtype instance Show (f (e (Hefty f e)) a) => Show (Hefty f e a)

overHefty ::
    (f (e (Hefty f e)) a -> f' (e' (Hefty f' e')) b) ->
    Hefty f e a ->
    Hefty f' e' b
overHefty f = Hefty . f . unHefty
{-# INLINE overHefty #-}

instance (Freer c fr, InjectIns e (e' (Hefty fr e'))) => SendIns e (Hefty fr e') where
    sendIns = Hefty . liftIns . injectIns
    {-# INLINE sendIns #-}

instance (Freer c fr, InjectSig e e') => SendSig e (Hefty fr e') where
    sendSig = Hefty . liftIns . injectSig
    {-# INLINE sendSig #-}

class InjectSig e (e' :: SigClass) where
    injectSig :: e f ~> e' f

instance (Freer c fr, InjectInsBy key e (e' (Hefty fr e'))) => SendInsBy key e (Hefty fr e') where
    sendInsBy = Hefty . liftIns . injectInsBy @key
    {-# INLINE sendInsBy #-}

instance (Freer c fr, InjectSigBy key e e') => SendSigBy key e (Hefty fr e') where
    sendSigBy = Hefty . liftIns . injectSigBy @key
    {-# INLINE sendSigBy #-}

class InjectSigBy key e (e' :: SigClass) | key e' -> e where
    injectSigBy :: e f ~> e' f
