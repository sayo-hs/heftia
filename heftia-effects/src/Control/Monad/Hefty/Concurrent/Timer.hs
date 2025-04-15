-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Interpreters for the [Timer]("Data.Effect.Concurrent.Timer") effects.
-}
module Control.Monad.Hefty.Concurrent.Timer (
    module Control.Monad.Hefty.Concurrent.Timer,
    module Data.Effect.Concurrent.Timer,
)
where

import Control.Monad.Hefty (
    Eff,
    FOEs,
    interpret,
    raise,
    raiseUnder,
    (&),
    (:>),
 )
import Control.Monad.Hefty.Coroutine (runCoroutine)
import Control.Monad.Hefty.State (evalState)
import Data.Effect.Concurrent.Timer
import Data.Effect.Coroutine (Status (Continue, Done))
import Data.Effect.State (get, put)
import Data.Time (DiffTime)
import Data.Void (Void, absurd)

runCyclicTimer
    :: forall a es
     . (Timer :> es, FOEs es)
    => Eff (CyclicTimer ': es) a
    -> Eff es a
runCyclicTimer a = do
    timer0 :: Status (Eff es) () DiffTime Void <- runCoroutine cyclicTimer
    a
        & raiseUnder
        & interpret \case
            Wait delta ->
                get @(Status (Eff es) () DiffTime Void) >>= \case
                    Done x -> absurd x
                    Continue () k -> put =<< raise (k delta)
        & evalState timer0
