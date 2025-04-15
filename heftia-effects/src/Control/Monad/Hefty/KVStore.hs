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
import Control.Monad.Hefty (CC, Eff, raiseUnder, (:>))
import Data.Effect.KVStore
import Data.Effect.State (runStateCC)
import Data.Map (Map)

runKVStoreCC
    :: forall k v a es ref
     . (Ord k, CC ref :> es)
    => Map k v
    -> Eff (KVStore k v ': es) a
    -> Eff es (Map k v, a)
runKVStoreCC initial =
    raiseUnder
        >>> runKVStoreAsState
        >>> runStateCC initial
{-# INLINE runKVStoreCC #-}
