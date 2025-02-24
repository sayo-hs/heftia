-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
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

import Control.Monad.Hefty (Eff, interpretBy, type (~>))
import Data.Effect.Coroutine
import Data.Effect.Input
import Data.Effect.Output

-- | Interpret the [coroutine]("Data.Effect.Coroutine")'s t'Yield' effect.
runCoroutine
    :: forall a b ans ef
     . Eff '[] (Yield a b ': ef) ans
    -> Eff '[] ef (Status (Eff '[] ef) a b ans)
runCoroutine = interpretBy (pure . Done) (\(Yield a) k -> pure $ Continue a k)
