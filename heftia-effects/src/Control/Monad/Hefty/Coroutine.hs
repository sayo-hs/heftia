-- SPDX-License-Identifier: MPL-2.0

module Control.Monad.Hefty.Coroutine (
    module Control.Monad.Hefty.Coroutine,
    module Data.Effect.Coroutine,
)
where

import Control.Monad.Hefty.Interpret (interpretBy)
import Control.Monad.Hefty.Types (Eff)
import Data.Effect.Coroutine

runCoroutine
    :: forall a b ans r
     . Eff '[] (Yield a b ': r) ans
    -> Eff '[] r (Status (Eff '[] r) a b ans)
runCoroutine = interpretBy (pure . Done) (\(Yield a) k -> pure $ Continue a k)
