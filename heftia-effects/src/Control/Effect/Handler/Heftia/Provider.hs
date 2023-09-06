-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Handler.Heftia.Provider where

import Control.Effect.Class (type (~>))
import Control.Effect.Class.Provider (ProviderS (Provide))
import Control.Monad.Trans (MonadTrans, lift)

-- | Elaborate the 'Provider' effect using the given interpreter for some monad transformer.
elaborateProviderT ::
    (Monad m, MonadTrans t, c (t m)) =>
    (forall x. i -> t m x -> m (g x)) ->
    ProviderS c i g m ~> m
elaborateProviderT run (Provide i m) = run i $ m lift
{-# INLINE elaborateProviderT #-}

-- | Elaborate the 'Provider' effect using the given interpreter.
elaborateProvider ::
    (Monad m, c m) =>
    (forall x. i -> m x -> m (g x)) ->
    ProviderS c i g m ~> m
elaborateProvider run (Provide i m) = run i $ m id
{-# INLINE elaborateProvider #-}
