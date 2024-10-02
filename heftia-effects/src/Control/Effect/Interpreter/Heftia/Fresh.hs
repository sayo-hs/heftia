-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Control.Effect.Interpreter.Heftia.Fresh where

import Control.Arrow ((>>>))
import Control.Effect (type (~>))
import Control.Effect.Interpreter.Heftia.State (runState)
import Control.Monad.Hefty.Interpret (interpretRec)
import Control.Monad.Hefty.Transform (raiseUnder)
import Control.Monad.Hefty.Types (Eff)
import Data.Effect.Fresh (Fresh (Fresh))
import Data.Effect.OpenUnion.Internal.FO (type (<|))
import Data.Effect.State (State, get, modify)
import Numeric.Natural (Natural)

runFreshNatural :: Eff '[] (Fresh Natural ': r) a -> Eff '[] r (Natural, a)
runFreshNatural =
    raiseUnder >>> runFreshNaturalAsState >>> runState 0

runFreshNaturalAsState
    :: (State Natural <| r)
    => Eff eh (Fresh Natural ': r) ~> Eff eh r
runFreshNaturalAsState =
    interpretRec \Fresh -> get @Natural <* modify @Natural (+ 1)
