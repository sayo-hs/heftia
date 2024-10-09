-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
Portability :  portable

This module provides handlers for the t`KVStore` effect, comes
from [@Polysemy.KVStore@](https://hackage.haskell.org/package/polysemy-kvstore-0.1.3.0/docs/Polysemy-KVStore.html)
in the @polysemy-kvstore@ package.
-}
module Control.Effect.Interpreter.Heftia.KVStore where

import Control.Arrow ((>>>))
import Control.Effect (type (~>))
import Control.Effect.Interpreter.Heftia.State (runState)
import Control.Monad.Hefty.Interpret (interpret)
import Control.Monad.Hefty.Transform (raiseUnder)
import Control.Monad.Hefty.Types (Eff)
import Data.Effect.KVStore (KVStore (LookupKV, UpdateKV))
import Data.Effect.OpenUnion.Internal.FO (type (<|))
import Data.Effect.State (State, get, modify)
import Data.Functor ((<&>))
import Data.Map (Map)
import Data.Map qualified as Map

runKVStorePure
    :: forall k v r a
     . (Ord k)
    => Map k v
    -> Eff '[] (KVStore k v ': r) a
    -> Eff '[] r (Map k v, a)
runKVStorePure initial =
    raiseUnder
        >>> runKVStoreAsState
        >>> runState initial

runKVStoreAsState
    :: forall k v r
     . (Ord k, State (Map k v) <| r)
    => Eff '[] (KVStore k v ': r) ~> Eff '[] r
runKVStoreAsState = interpret \case
    LookupKV k -> get <&> Map.lookup k
    UpdateKV k v -> modify $ Map.update (const v) k
