-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Interpreter.Heftia.Coroutine where

import Control.Effect.Hefty (Eff, interpretK)
import Control.Monad.Freer (MonadFreer)
import Data.Effect.Coroutine (LYield, Status (Coroutine, Done), Yield (Yield))
import Data.Hefty.Union (Union)

runCoroutine ::
    forall a b r er fr u c.
    (MonadFreer c fr, Union u, c (Eff u fr '[] er)) =>
    Eff u fr '[] (LYield a b ': er) r ->
    Eff u fr '[] er (Status (Eff u fr '[] er) a b r)
runCoroutine = interpretK (pure . Done) (\kk (Yield a) -> pure $ Coroutine a kk)
