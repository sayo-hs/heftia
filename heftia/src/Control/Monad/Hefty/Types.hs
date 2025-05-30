{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

This module defines the t'Eff' monad and related fundamental types and functions.
Please refer to the documentation of the [top-level module]("Control.Monad.Hefty").
-}
module Control.Monad.Hefty.Types where

import Control.Effect (Free)
import Control.Effect qualified as D
import Data.Effect (Effect)
import Data.FTCQueue (FTCQueue, ViewL (..), tsingleton, tviewl, (><), (|>))
import Data.Kind (Type)

data Freer f a
    = -- | A pure value.
      Val a
    | -- | An effectful operation.
      forall x. Op
        (f x)
        (FTCQueue (Freer f) x a)
        -- ^ the continuation of the operation.

type Eff = D.Eff Freer

type AlgHandler (e :: Effect) m n (ans :: Type) = forall x. e m x -> (x -> n ans) -> n ans

instance Functor (Freer f) where
    fmap f = \case
        Val x -> Val (f x)
        Op u q -> Op u (q |> (Val . f))
    {-# INLINE fmap #-}

instance Applicative (Freer f) where
    pure = Val
    {-# INLINE pure #-}

    Val f <*> Val x = Val $ f x
    Val f <*> Op u q = Op u (q |> (Val . f))
    Op u q <*> m = Op u (q |> (<$> m))
    {-# INLINE (<*>) #-}

instance Monad (Freer f) where
    m >>= k = case m of
        Val x -> k x
        Op e q -> Op e (q |> k)
    {-# INLINE (>>=) #-}

instance Free Monad Freer where
    liftFree f = Op f (tsingleton pure)
    runFree i = loop
      where
        loop = \case
            Val x -> pure x
            Op f q -> i f >>= loop . qApp q

    {-# INLINE liftFree #-}
    {-# INLINE runFree #-}

-- | Applies a value to a Kleisli arrow in 'FTCQueue' representation.
qApp :: FTCQueue (Freer f) a b -> a -> Freer f b
qApp q' x = case tviewl q' of
    TOne k -> k x
    k :| t -> case k x of
        Val y -> qApp t y
        Op u q -> Op u (q >< t)
