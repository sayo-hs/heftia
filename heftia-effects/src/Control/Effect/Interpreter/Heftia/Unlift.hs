-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
Portability :  portable
-}
module Control.Effect.Interpreter.Heftia.Unlift where

import Control.Effect (type (~>))
import Control.Monad.Hefty (Eff, interpretH, runEff, send0)
import Data.Effect.Unlift (UnliftBase (WithRunInBase), UnliftIO)

runUnliftBase :: forall b. (Monad b) => Eff '[UnliftBase b] '[b] ~> b
runUnliftBase =
    runEff . interpretH \(WithRunInBase f) ->
        send0 $ f runEff

runUnliftIO :: Eff '[UnliftIO] '[IO] ~> IO
runUnliftIO = runUnliftBase
{-# INLINE runUnliftIO #-}
