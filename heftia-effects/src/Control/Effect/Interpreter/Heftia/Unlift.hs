-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Control.Effect.Interpreter.Heftia.Unlift where

import Control.Effect (type (~>))
import Control.Effect.Hefty (Eff, interpretH, runEff, send0)
import Control.Freer (Freer)
import Data.Effect (LiftIns)
import Data.Effect.Unlift (UnliftBase (WithRunInBase), UnliftIO)
import Data.Hefty.Union (Union)

runUnliftBase
    :: forall b fr u c
     . (Freer c fr, Union u, c b)
    => Eff u fr '[UnliftBase b] '[LiftIns b] ~> b
runUnliftBase =
    runEff . interpretH \(WithRunInBase f) ->
        send0 $ f runUnliftBase

runUnliftIO
    :: forall fr u c
     . (Freer c fr, Union u, c IO)
    => Eff u fr '[UnliftIO] '[LiftIns IO] ~> IO
runUnliftIO = runUnliftBase
{-# INLINE runUnliftIO #-}
