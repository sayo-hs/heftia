-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Interpreter.Heftia.ShiftReset where

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
import Data.Effect.ShiftReset (
    Reset (Reset),
    Shift,
    Shift' (Shift),
    Shift_,
    Shift_' (Shift_'),
 )

type ShiftFix ans eh ef = Shift ans (ShiftBase ans eh ef)

newtype ShiftBase ans eh ef a
    = ShiftBase {unShiftBase :: Eff (Shift ans (ShiftBase ans eh ef) ': eh) ef a}
    deriving newtype (Functor, Applicative, Monad)

evalShift :: Eff '[ShiftFix ans '[] ef] ef ans -> Eff '[] ef ans
evalShift = runShift pure

runShift :: (a -> Eff '[] ef ans) -> Eff '[ShiftFix ans '[] ef] ef a -> Eff '[] ef ans
runShift f =
    interpretHBy f \e k ->
        evalShift $ case e of
            KeyH (Shift g) -> unShiftBase $ g (ShiftBase . raiseH . k) ShiftBase

withShift :: Eff '[ShiftFix ans '[] '[Eff eh ef]] '[Eff eh ef] ans -> Eff eh ef ans
withShift = runEff . evalShift

runShift_ :: forall r ef. Eff (Shift_ (Eff r ef) ': r) ef ~> Eff r ef
runShift_ = interpretRecHWith \(KeyH (Shift_' f)) k -> f k id

runReset :: forall r ef. Eff (Reset ': r) ef ~> Eff r ef
runReset = interpretH \(Reset a) -> a
