-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024-2025 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
-}
module Control.Monad.Hefty.Fresh (
    module Control.Monad.Hefty.Fresh,
    module Data.Effect.Fresh,
) where

import Control.Arrow ((>>>))
import Control.Monad.Hefty (Eff, FOEs, interpret, raiseUnder, (:>))
import Control.Monad.Hefty.State (runState)
import Data.Effect.Fresh
import Data.Effect.State (State, get, modify)
import Numeric.Natural (Natural)

runFreshNatural :: (FOEs es) => Eff (Fresh Natural ': es) a -> Eff es (Natural, a)
runFreshNatural =
    raiseUnder >>> runFreshNaturalAsState >>> runState 0
{-# INLINE runFreshNatural #-}

runFreshNaturalAsState
    :: (State Natural :> es)
    => Eff (Fresh Natural ': es) a
    -> Eff es a
runFreshNaturalAsState =
    interpret \Fresh -> get <* modify (+ 1)
{-# INLINE runFreshNaturalAsState #-}
