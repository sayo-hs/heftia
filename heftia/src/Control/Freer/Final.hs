{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fmap" #-}
{-# HLINT ignore "Use const" #-}
{-# HLINT ignore "Avoid lambda" #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

A final-encoded generic Freer carrier.
-}
module Control.Freer.Final where

import Control.Applicative (Alternative, empty, liftA2, many, some, (<|>))
import Control.Effect (type (~>))
import Control.Freer (Freer, interpretFreer, liftIns)
import Control.Monad (MonadPlus, mplus, mzero)
import Control.Monad.Freer (MonadFreer)

-- | A final-encoded generic Freer carrier.
newtype FreerFinal c f a = FreerFinal {unFreerFinal :: forall m. c m => (f ~> m) -> m a}

deriving stock instance (forall f. c f => Functor f) => Functor (FreerFinal c e)

instance
    (forall f. c f => Applicative f, Functor (FreerFinal c e)) =>
    Applicative (FreerFinal c e)
    where
    pure x = FreerFinal \_ -> pure x

    FreerFinal f <*> FreerFinal g =
        FreerFinal \i -> f i <*> g i

    liftA2 f (FreerFinal fa) (FreerFinal fb) =
        FreerFinal \i -> liftA2 f (fa i) (fb i)

    FreerFinal f *> FreerFinal g =
        FreerFinal \i -> f i *> g i

    FreerFinal f <* FreerFinal g =
        FreerFinal \i -> f i <* g i

    {-# INLINE pure #-}
    {-# INLINE (<*>) #-}
    {-# INLINE liftA2 #-}
    {-# INLINE (*>) #-}
    {-# INLINE (<*) #-}

instance
    (forall f. c f => Alternative f, Applicative (FreerFinal c e)) =>
    Alternative (FreerFinal c e)
    where
    empty = FreerFinal \_ -> empty

    FreerFinal f <|> FreerFinal g =
        FreerFinal \i -> f i <|> g i

    some (FreerFinal f) = FreerFinal \i -> some (f i)
    many (FreerFinal f) = FreerFinal \i -> many (f i)

    {-# INLINE empty #-}
    {-# INLINE (<|>) #-}
    {-# INLINE some #-}
    {-# INLINE many #-}

instance (forall m. c m => Monad m, Applicative (FreerFinal c f)) => Monad (FreerFinal c f) where
    FreerFinal f >>= k =
        FreerFinal \i ->
            f i >>= interpretFreerFinal i . k

    (>>) = (*>)
    return = pure

    {-# INLINE (>>=) #-}
    {-# INLINE (>>) #-}
    {-# INLINE return #-}

instance
    (forall m. c m => MonadPlus m, Alternative (FreerFinal c f), Monad (FreerFinal c f)) =>
    MonadPlus (FreerFinal c f)
    where
    mzero = FreerFinal \_ -> mzero

    FreerFinal f `mplus` FreerFinal g =
        FreerFinal \i -> f i `mplus` g i

    {-# INLINE mzero #-}
    {-# INLINE mplus #-}

interpretFreerFinal :: c f => (e ~> f) -> FreerFinal c e a -> f a
interpretFreerFinal i (FreerFinal f) = f i
{-# INLINE interpretFreerFinal #-}

liftInsFinal :: ins a -> FreerFinal c ins a
liftInsFinal e = FreerFinal \i -> i e
{-# INLINE liftInsFinal #-}

instance (forall e. c (FreerFinal c e)) => Freer c (FreerFinal c) where
    liftIns = liftInsFinal
    interpretFreer = interpretFreerFinal
    {-# INLINE liftIns #-}
    {-# INLINE interpretFreer #-}

instance MonadFreer Monad (FreerFinal Monad)
