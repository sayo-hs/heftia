-- SPDX-License-Identifier: MPL-2.0

module Control.Effect.Interpreter.Heftia.Coroutine where

import Control.Monad.Hefty.Interpret (interpretBy)
import Control.Monad.Hefty.Types (Eff)
import Data.Effect.Coroutine (Status (Coroutine, Done), Yield (Yield))

runCoroutine
    :: forall a b ans r
     . Eff '[] (Yield a b ': r) ans
    -> Eff '[] r (Status (Eff '[] r) a b ans)
runCoroutine = interpretBy (pure . Done) (\(Yield a) k -> pure $ Coroutine a k)
