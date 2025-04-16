-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2025 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
-}
module Control.Monad.Hefty.Shift (
    module Control.Monad.Hefty.Shift,
    module Data.Effect.Shift,
)
where

import Control.Monad.Hefty (AlgHandler, Eff, interpretBy)
import Data.Effect.OpenUnion (FOEs)
import Data.Effect.Shift
import Data.Functor.Contravariant (Op (Op))

runShift :: (FOEs es) => (a -> Eff es ans) -> Eff (Shift ans (Op (Eff es ans)) ': es) a -> Eff es ans
runShift k = interpretBy k handleShift
{-# INLINE runShift #-}

handleShift :: (Monad m) => AlgHandler (Shift ans (Op (m ans))) n m ans
handleShift = \case
    SubShiftFork -> \exit -> exit . Left . Op $ exit . Right
    Call (Op exit) x -> (exit x >>=)
    Abort ans -> const $ pure ans
{-# INLINE handleShift #-}

evalShift :: (FOEs es) => Eff (Shift a (Op (Eff es a)) ': es) a -> Eff es a
evalShift = runShift pure
{-# INLINE evalShift #-}
