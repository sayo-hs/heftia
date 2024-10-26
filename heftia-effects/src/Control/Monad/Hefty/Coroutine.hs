-- SPDX-License-Identifier: MPL-2.0

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

runCoroutine
    :: forall a b ans ef
     . Eff '[] (Yield a b ': ef) ans
    -> Eff '[] ef (Status (Eff '[] ef) a b ans)
runCoroutine = interpretBy (pure . Done) (\(Yield a) k -> pure $ Continue a k)

inputToYield :: Input i ~> Yield () i
inputToYield Input = Yield ()

outputToYield :: Output o ~> Yield o ()
outputToYield (Output o) = Yield o
