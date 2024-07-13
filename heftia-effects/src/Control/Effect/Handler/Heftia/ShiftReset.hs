-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Handler.Heftia.ShiftReset where

import Control.Arrow ((>>>))
import Control.Effect (type (~>))
import Control.Effect.Hefty (
    Eff,
    injectH,
    interpretKAllH_,
    interpretKH_,
    interpretRecH,
    raiseH,
    runEff,
 )
import Control.Freer (Freer)
import Control.Monad ((<=<))
import Control.Monad.Freer (MonadFreer)
import Data.Effect (LiftIns)
import Data.Effect.HFunctor (HFunctor, hfmap)
import Data.Effect.ShiftReset (Reset (Reset), Shift (Shift), Shift_ (Shift_))
import Data.Hefty.Union (HFunctorUnion, HFunctorUnion_ (ForallHFunctor), Union ((|+:)))

runShift ::
    (MonadFreer c fr, Union u, c (Eff u fr '[] ef), HFunctor (u '[])) =>
    Eff u fr '[Shift r] ef r ->
    Eff u fr '[] ef r
runShift =
    interpretKH_ pure \k ->
        let k' = raiseH . k
         in runShift . \case
                Shift f -> f k'

withShift ::
    ( MonadFreer c fr
    , Union u
    , c (Eff u fr '[] '[LiftIns (Eff u fr eh ef)])
    , c (Eff u fr eh ef)
    , HFunctor (u '[])
    ) =>
    Eff u fr '[Shift r] '[LiftIns (Eff u fr eh ef)] r ->
    Eff u fr eh ef r
withShift = runShift >>> runEff
{-# INLINE withShift #-}

runShift_ ::
    (MonadFreer c fr, Union u, c (Eff u fr eh ef), HFunctor (u eh)) =>
    Eff u fr (Shift_ ': eh) ef ~> Eff u fr eh ef
runShift_ =
    interpretKAllH_ pure \k ->
        (\(Shift_ f) -> runShift_ $ f $ raiseH . k)
            |+: (k <=< injectH . hfmap runShift_)

runReset ::
    (Freer c fr, HFunctorUnion u, ForallHFunctor u eh) =>
    Eff u fr (Reset ': eh) ef ~> Eff u fr eh ef
runReset = interpretRecH \(Reset a) -> a
{-# INLINE runReset #-}
