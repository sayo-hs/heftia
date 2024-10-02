-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Interpreter.Heftia.ShiftReset where

import Control.Effect (type (~>))
import Control.Monad.Hefty (runEff)
import Control.Monad.Hefty.Interpret (interpretHBy_, interpretRecH, iterAllEffHFBy)
import Control.Monad.Hefty.Transform (raiseH)
import Control.Monad.Hefty.Types (Eff, sendUnionBy, sendUnionHBy)
import Data.Effect.Key (KeyH (KeyH))
import Data.Effect.OpenUnion.Internal.HO (HFunctors, hfmapUnion, (!!+.))
import Data.Effect.ShiftReset (Reset (Reset), Shift, Shift' (Shift), Shift_ (Shift_))

evalShift :: Eff '[Shift ans] ef ans -> Eff '[] ef ans
evalShift = runShift pure

runShift :: (a -> Eff '[] ef ans) -> Eff '[Shift ans] ef a -> Eff '[] ef ans
runShift f =
    interpretHBy_ f \e k ->
        let k' = raiseH . k
         in evalShift $ case e of
                KeyH (Shift g) -> g k'

withShift :: Eff '[Shift ans] '[Eff eh ef] ans -> Eff eh ef ans
withShift = runEff . evalShift

runShift_ :: forall r ef. (HFunctors r) => Eff (Shift_ ': r) ef ~> Eff r ef
runShift_ =
    iterAllEffHFBy
        pure
        ( (\(Shift_ f) k -> runShift_ $ f $ raiseH . k)
            !!+. (flip sendUnionHBy . hfmapUnion runShift_)
        )
        (flip sendUnionBy)

runReset :: forall r ef. (HFunctors r) => Eff (Reset ': r) ef ~> Eff r ef
runReset = interpretRecH \(Reset a) -> a
