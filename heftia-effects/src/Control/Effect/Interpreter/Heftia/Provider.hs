-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

Elaborator for the t'Control.Effect.Class.Provider.Provider' effect class.
-}
module Control.Effect.Interpreter.Heftia.Provider where

import Control.Effect (type (~>))
import Control.Effect.Hefty (Elab)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Effect.Provider (Provider' (Provide))

-- | Elaborate the t'Control.Effect.Class.Provider.Provider' effect using the given interpreter.
runProvider ::
    (c g, e g) =>
    (f ~> g) ->
    (i -> forall x. g x -> f (ctx x)) ->
    Elab (Provider' c i ctx e) f
runProvider iLower run (Provide i f) = run i $ f iLower
{-# INLINE runProvider #-}

{- |
Elaborate the t'Control.Effect.Class.Provider.Provider' effect using the given interpreter for some
monad transformer.
-}
runProviderT ::
    (Monad m, MonadTrans t, c (t m), e (t m)) =>
    (i -> forall x. t m x -> m (ctx x)) ->
    Elab (Provider' c i ctx e) m
runProviderT = runProvider lift
{-# INLINE runProviderT #-}
