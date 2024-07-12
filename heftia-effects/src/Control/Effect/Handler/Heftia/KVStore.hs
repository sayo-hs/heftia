-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

This module provides handlers for the t`KVStore` effect, comes
from [@Polysemy.KVStore@](https://hackage.haskell.org/package/polysemy-kvstore-0.1.3.0/docs/Polysemy-KVStore.html)
in the @polysemy-kvstore@ package.
-}
module Control.Effect.Handler.Heftia.KVStore where

import Control.Arrow ((>>>))
import Control.Effect (type (~>))
import Control.Effect.Handler.Heftia.State (interpretState)
import Control.Effect.Hefty (Eff, interpret, raiseUnder)
import Control.Freer (Freer)
import Control.Monad.State (StateT)
import Data.Effect.HFunctor (HFunctor)
import Data.Effect.KVStore (KVStore (LookupKV, UpdateKV), LKVStore)
import Data.Effect.State (LState, State, get, modify)
import Data.Functor ((<&>))
import Data.Hefty.Union (Member, Union)
import Data.Map (Map)
import Data.Map qualified as Map

runKVStorePure ::
    forall k v r a fr u c.
    ( Ord k
    , Freer c fr
    , Union u
    , HFunctor (u '[])
    , Member u (State (Map k v)) (LState (Map k v) ': r)
    , c (Eff u fr '[] r)
    , c (StateT (Map k v) (Eff u fr '[] r))
    , Monad (Eff u fr '[] r)
    , Monad (Eff u fr '[] (LState (Map k v) ': r))
    ) =>
    Map k v ->
    Eff u fr '[] (LKVStore k v ': r) a ->
    Eff u fr '[] r (Map k v, a)
runKVStorePure initial =
    raiseUnder
        >>> runKVStoreAsState
        >>> interpretState initial
{-# INLINE runKVStorePure #-}

runKVStoreAsState ::
    forall k v r fr u c.
    ( Ord k
    , Freer c fr
    , Union u
    , Member u (State (Map k v)) r
    , Monad (Eff u fr '[] r)
    , HFunctor (u '[])
    ) =>
    Eff u fr '[] (LKVStore k v ': r) ~> Eff u fr '[] r
runKVStoreAsState = interpret \case
    LookupKV k -> get <&> Map.lookup k
    UpdateKV k v -> modify $ Map.update (const v) k
