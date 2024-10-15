-- SPDX-License-Identifier: MPL-2.0

module Control.Monad.Hefty.Coroutine (
    module Control.Monad.Hefty.Coroutine,
    module Data.Effect.Coroutine,
)
where

import Control.Monad.Hefty (Eff, interpretBy)
import Data.Effect.Coroutine

runCoroutine
    :: forall a b ans ef
     . Eff '[] (Yield a b ': ef) ans
    -> Eff '[] ef (Status (Eff '[] ef) a b ans)
runCoroutine = interpretBy (pure . Done) (\(Yield a) k -> pure $ Continue a k)
