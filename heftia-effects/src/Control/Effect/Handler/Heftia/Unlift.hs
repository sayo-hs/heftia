-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Handler.Heftia.Unlift where

import Control.Freer (Freer)
import Data.Hefty.Union (Union)
import Control.Effect.Hefty (Eff, interpretH_, send0, runEff)
import Data.Effect.Unlift (UnliftBase (WithRunInBase), UnliftIO)
import Control.Effect (type (~>))
import Data.Effect (LiftIns)

runUnliftBase ::
    forall b fr u c.
    (Freer c fr, Union u, c b) =>
    Eff u fr '[UnliftBase b] '[LiftIns b] ~> b
runUnliftBase =
    runEff . interpretH_ \(WithRunInBase f) ->
        send0 $ f runUnliftBase

runUnliftIO ::
    forall fr u c.
    (Freer c fr, Union u, c IO) =>
    Eff u fr '[UnliftIO] '[LiftIns IO] ~> IO
runUnliftIO = runUnliftBase
{-# INLINE runUnliftIO #-}
