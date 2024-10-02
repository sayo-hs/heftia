-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

Interpreter and elaborator for the t'Data.Effect.Reader.Local' / t'Data.Effect.Reader.Catch' effect
classes.
-}
module Control.Effect.Interpreter.Heftia.Reader where

import Control.Effect (type (~>))
import Control.Monad.Hefty (
    Eff,
    Elab,
    HFunctors,
    interposeRec,
    interpretRec,
    interpretRecH,
    type (<|),
 )
import Data.Effect.Reader (Ask (..), Local (..), ask)
import Data.Function ((&))

runReader
    :: forall r eh ef
     . (HFunctors eh)
    => r
    -> Eff (Local r ': eh) (Ask r ': ef) ~> Eff eh ef
runReader r = runAsk r . runLocal

-- | Elaborate the t'Local' effect.
runLocal
    :: forall r eh ef
     . (Ask r <| ef, HFunctors eh)
    => Eff (Local r ': eh) ef ~> Eff eh ef
runLocal = interpretRecH elabLocal

elabLocal
    :: forall r eh ef
     . (Ask r <| ef, HFunctors eh)
    => Elab (Local r) (Eff eh ef)
elabLocal (Local f a) = a & interposeRec @(Ask r) \Ask -> f <$> ask

-- | Interpret the t'Ask' effect.
runAsk
    :: forall r ef eh
     . (HFunctors eh)
    => r
    -> Eff eh (Ask r ': ef) ~> Eff eh ef
runAsk r = interpretRec \Ask -> pure r
