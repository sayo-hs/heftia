-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Handler.Heftia.Coroutine where

import Data.Effect.Coroutine (continueStatus, replyCoroutine, LYield, Status (Done, Coroutine), YieldH (YieldH))
import Control.Effect.Hefty (Eff, interpretK, interpretKH_)
import Control.Monad.Freer (MonadFreer)
import Data.Hefty.Union (Union)

runCoroutine ::
    forall a b r er fr u c.
    (MonadFreer c fr, Union u, c (Eff u fr '[] er)) =>
    Eff u fr '[] (LYield a b ': er) r -> Eff u fr '[] er (Status (Eff u fr '[] er) a b r)
runCoroutine = interpretK (pure . Done) (\kk y -> pure $ replyCoroutine y kk)

runCoroutineH ::
    forall a b r ef fr u c.
    (MonadFreer c fr, Union u, c (Eff u fr '[] ef)) =>
    Eff u fr '[YieldH a b] ef r -> Eff u fr '[] ef (Status (Eff u fr '[] ef) a b r)
runCoroutineH =
    interpretKH_ (pure . Done) \kk (YieldH a k) ->
        pure $ Coroutine a \b ->
            runCoroutineH (k b) >>= continueStatus kk
