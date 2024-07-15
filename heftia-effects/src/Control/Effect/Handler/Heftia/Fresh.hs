-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Control.Effect.Handler.Heftia.Fresh where

import Control.Arrow ((>>>))
import Control.Effect (type (~>))
import Control.Effect.Handler.Heftia.State (runState)
import Control.Effect.Hefty (Eff, interpret, raiseUnder)
import Control.Freer (Freer)
import Control.Monad.State (StateT)
import Data.Effect.Fresh (Fresh (Fresh), LFresh)
import Data.Effect.HFunctor (HFunctor)
import Data.Effect.State (LState, State, get, modify)
import Data.Hefty.Union (Member, Union)
import Numeric.Natural (Natural)

runFreshNatural ::
    ( Freer c fr
    , Union u
    , HFunctor (u '[])
    , Member u (State Natural) (LState Natural ': r)
    , c (Eff u fr '[] r)
    , c (StateT Natural (Eff u fr '[] r))
    , Monad (Eff u fr '[] r)
    , Monad (Eff u fr '[] (LState Natural ': r))
    ) =>
    Eff u fr '[] (LFresh Natural ': r) a ->
    Eff u fr '[] r (Natural, a)
runFreshNatural =
    raiseUnder
        >>> runFreshNaturalAsState
        >>> runState 0
{-# INLINE runFreshNatural #-}

runFreshNaturalAsState ::
    forall r fr u c.
    ( Freer c fr
    , Union u
    , Member u (State Natural) r
    , Monad (Eff u fr '[] r)
    , HFunctor (u '[])
    ) =>
    Eff u fr '[] (LFresh Natural ': r) ~> Eff u fr '[] r
runFreshNaturalAsState =
    interpret \Fresh -> get @Natural <* modify @Natural (+ 1)
