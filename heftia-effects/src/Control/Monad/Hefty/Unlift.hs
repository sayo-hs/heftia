-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Interpreters for the [Unlift]("Data.Effect.Unlift") effects.
-}
module Control.Monad.Hefty.Unlift (
    module Control.Monad.Hefty.Unlift,
    module Data.Effect.Unlift,
)
where

import Control.Monad.Hefty (Eff, interpretH, runEff, send0, type (~>))
import Data.Effect.Unlift
import UnliftIO (MonadUnliftIO)
import UnliftIO qualified as IO

runUnliftBase :: forall b. (Monad b) => Eff '[UnliftBase b] '[b] ~> b
runUnliftBase =
    runEff . interpretH \(WithRunInBase f) ->
        send0 $ f runEff

runUnliftIO :: (MonadUnliftIO m) => Eff '[UnliftIO] '[m] ~> m
runUnliftIO =
    runEff . interpretH \(WithRunInBase f) ->
        send0 $ IO.withRunInIO \run -> f $ run . runEff
