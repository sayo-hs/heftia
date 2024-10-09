-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
Portability :  portable
-}
module Control.Monad.Hefty.Fresh where

import Control.Arrow ((>>>))
import Control.Monad.Hefty (Eff, interpret, raiseUnder, type (<|), type (~>))
import Control.Monad.Hefty.State (runState)
import Data.Effect.Fresh (Fresh (Fresh))
import Data.Effect.State (State, get, modify)
import Numeric.Natural (Natural)

runFreshNatural :: Eff '[] (Fresh Natural ': r) a -> Eff '[] r (Natural, a)
runFreshNatural =
    raiseUnder >>> runFreshNaturalAsState >>> runState 0

runFreshNaturalAsState
    :: (State Natural <| r)
    => Eff eh (Fresh Natural ': r) ~> Eff eh r
runFreshNaturalAsState =
    interpret \Fresh -> get @Natural <* modify @Natural (+ 1)
