-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024-2025 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Realizes [@unliftio@](https://hackage.haskell.org/package/unliftio) in the form of higher-order effects.
-}
module Control.Monad.Hefty.Unlift (module Control.Monad.Hefty.Unlift, module Data.Effect.Unlift) where

import Control.Monad.Hefty (Eff, Emb)
import Data.Effect.Unlift hiding (runUnliftBase, runUnliftIO)
import Data.Effect.Unlift qualified as G
import UnliftIO (MonadUnliftIO)

runUnliftBase :: (Monad m) => Eff '[UnliftBase m, Emb m] a -> m a
runUnliftBase = G.runUnliftBase
{-# INLINE runUnliftBase #-}

runUnliftIO :: (MonadUnliftIO m) => Eff '[UnliftIO, Emb m] a -> m a
runUnliftIO = G.runUnliftIO
{-# INLINE runUnliftIO #-}
