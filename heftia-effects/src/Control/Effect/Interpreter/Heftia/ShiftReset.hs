-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Control.Effect.Interpreter.Heftia.ShiftReset where

import Control.Effect (type (~>))
import Control.Monad.Hefty.Interpret (interpretRecH, iterAllEffHFBy)
import Control.Monad.Hefty.Transform (raiseH)
import Control.Monad.Hefty.Types (Eff, sendUnionBy, sendUnionHBy)
import Data.Effect.HFunctor.HCont (HCont (HCont))
import Data.Effect.OpenUnion.Internal.HO (hfmapUnion, (!!+))
import Data.Effect.ShiftReset (Reset (Reset), Shift_ (Shift_))

runShift_ :: forall r ef. Eff (HCont Shift_ (Eff r ef) ': r) ef ~> Eff r ef
runShift_ =
    iterAllEffHFBy
        pure
        ( ( \(HCont g) k ->
                case g runShift_ of
                    Shift_ f -> runShift_ $ raiseH $ f k
          )
            !!+ (flip sendUnionHBy . hfmapUnion runShift_)
        )
        (flip sendUnionBy)

runReset :: forall r ef. Eff (Reset ': r) ef ~> Eff r ef
runReset = interpretRecH \(Reset a) -> a
{-# INLINE runReset #-}
