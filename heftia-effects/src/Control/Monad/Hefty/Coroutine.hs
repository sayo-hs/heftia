-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024-2025 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Interpreters for the [coroutine]("Data.Effect.Coroutine") effect.
-}
module Control.Monad.Hefty.Coroutine (
    module Control.Monad.Hefty.Coroutine,
    module Data.Effect.Coroutine,
    module Data.Effect.Input,
    module Data.Effect.Output,
)
where

import Control.Monad.Hefty (Eff, FOEs, interpretBy)
import Data.Effect.Coroutine
import Data.Effect.Input
import Data.Effect.Output

-- | Interpret the [coroutine]("Data.Effect.Coroutine")'s t'Yield' effect.
runCoroutine
    :: forall ans a b es
     . (FOEs es)
    => Eff (Yield a b ': es) ans
    -> Eff es (Status (Eff es) a b ans)
runCoroutine = interpretBy (pure . Done) (\(Yield a) k -> pure $ Continue a k)
