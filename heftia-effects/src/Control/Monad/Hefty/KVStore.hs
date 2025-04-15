-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024-2025 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

This module provides handlers for the t`KVStore` effect, comes
from [@Polysemy.KVStore@](https://hackage.haskell.org/package/polysemy-kvstore-0.1.3.0/docs/Polysemy-KVStore.html)
in the @polysemy-kvstore@ package.
-}
module Control.Monad.Hefty.KVStore (
    module Control.Monad.Hefty.KVStore,
    module Data.Effect.KVStore,
)
where

import Control.Arrow ((>>>))
import Control.Monad.Hefty (Eff, FOEs, raiseUnder)
import Control.Monad.Hefty.State (runState)
import Data.Effect.KVStore
import Data.Map (Map)

runKVStoreCC
    :: forall k v a es
     . (Ord k, FOEs es)
    => Map k v
    -> Eff (KVStore k v ': es) a
    -> Eff es (Map k v, a)
runKVStoreCC initial =
    raiseUnder
        >>> runKVStoreAsState
        >>> runState initial
{-# INLINE runKVStoreCC #-}
