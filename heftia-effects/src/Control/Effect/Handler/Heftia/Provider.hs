-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Handler.Heftia.Provider where

import Control.Effect.Class (type (~>))
import Control.Effect.Class.Provider (ProviderS (Provide))
import Control.Effect.Heftia (Elaborator)
import Control.Monad.Trans (MonadTrans, lift)

-- | Elaborate the 'Provider' effect using the given interpreter.
elaborateProvider ::
    c h =>
    (f ~> h) ->
    (forall x. i -> h x -> f (g x)) ->
    Elaborator (ProviderS c i g) f
elaborateProvider iLower run (Provide i a) = run i $ a iLower
{-# INLINE elaborateProvider #-}

-- | Elaborate the 'Provider' effect using the given interpreter for some monad transformer.
elaborateProviderT ::
    (Monad m, MonadTrans t, c (t m)) =>
    (forall x. i -> t m x -> m (g x)) ->
    Elaborator (ProviderS c i g) m
elaborateProviderT = elaborateProvider lift
{-# INLINE elaborateProviderT #-}
