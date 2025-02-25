-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Monad.Hefty.ShiftReset (
    module Control.Monad.Hefty.ShiftReset,
    module Data.Effect.ShiftReset,
)
where

import Control.Monad.Hefty (
    Eff,
    interpretH,
    interpretHBy,
    interpretRecHWith,
    raiseH,
    runEff,
    type (~>),
 )
import Data.Effect.Key (KeyH (KeyH))
import Data.Effect.ShiftReset (Shift' (Shift))
import Data.Effect.ShiftReset hiding (Shift)
import Data.Effect.ShiftReset qualified as D

type Shift ans eh ef = D.Shift ans (ShiftEff ans eh ef)

newtype ShiftEff ans eh ef a
    = ShiftEff {unShiftEff :: Eff (D.Shift ans (ShiftEff ans eh ef) ': eh) ef a}
    deriving newtype (Functor, Applicative, Monad)

evalShift :: Eff '[Shift ans '[] ef] ef ans -> Eff '[] ef ans
evalShift = runShift pure

runShift :: (a -> Eff '[] ef ans) -> Eff '[Shift ans '[] ef] ef a -> Eff '[] ef ans
runShift f =
    interpretHBy f \e k ->
        evalShift $ case e of
            KeyH (Shift initiate) -> unShiftEff $ initiate (ShiftEff . raiseH . k) ShiftEff

withShift :: Eff '[Shift ans '[] '[Eff eh ef]] '[Eff eh ef] ans -> Eff eh ef ans
withShift = runEff . evalShift

runReset :: forall eh ef. Eff (Reset ': eh) ef ~> Eff eh ef
runReset = interpretH \(Reset a) -> a
