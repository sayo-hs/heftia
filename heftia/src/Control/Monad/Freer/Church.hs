-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

A Church-encoded Freer monad.
-}
module Control.Monad.Freer.Church where

import Control.Effect.Class (type (~>))
import Control.Freer (Freer, interpretFreer, liftIns, retractFreer, transformFreer)
import Control.Monad.Cont (Cont, ContT (ContT), runCont)
import Control.Monad.Freer (MonadFreer, interpretFreerK)
import Control.Monad.Identity (Identity (Identity), runIdentity)
import Control.Monad.Trans.Free.Church (FT (FT), retract, transFT)

-- | A Church encoded Freer monad.
newtype FreerChurch f a = FreerChurch {unFreerChurch :: FT f Identity a}
    deriving newtype
        ( Functor
        , Applicative
        , Monad
        , Eq
        , Ord
        )
    deriving stock (Foldable, Traversable)

liftInsChurch :: ins a -> FreerChurch ins a
liftInsChurch e = FreerChurch $ FT \k f -> f k e
{-# INLINE liftInsChurch #-}

interpretChurch :: Monad m => (ins ~> m) -> FreerChurch ins a -> m a
interpretChurch i = retract . transFT i . unFreerChurch
{-# INLINE interpretChurch #-}

interpretChurchK :: (e ~> Cont r) -> FreerChurch e ~> Cont r
interpretChurchK i (FreerChurch (FT f)) =
    ContT \k -> f k \k' e -> Identity $ runCont (i e) (runIdentity . k')

instance Freer Monad FreerChurch where
    liftIns = liftInsChurch
    interpretFreer = interpretChurch
    retractFreer = retract . unFreerChurch
    transformFreer phi = FreerChurch . transFT phi . unFreerChurch
    {-# INLINE liftIns #-}
    {-# INLINE interpretFreer #-}
    {-# INLINE retractFreer #-}
    {-# INLINE transformFreer #-}

instance MonadFreer FreerChurch where
    interpretFreerK = interpretChurchK
    {-# INLINE interpretFreerK #-}
