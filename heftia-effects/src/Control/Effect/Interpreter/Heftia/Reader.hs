-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
Portability :  portable

Interpreters for the t'Ask' / t'Local' effects.
-}
module Control.Effect.Interpreter.Heftia.Reader where

import Control.Monad.Hefty (
    Eff,
    interpose,
    interpret,
    interpretH,
    (&),
    type (<|),
    type (~>),
    type (~~>),
 )
import Data.Effect.Reader (Ask (..), Local (..), ask)

runReader
    :: forall r eh ef
     . r
    -> Eff (Local r ': eh) (Ask r ': ef) ~> Eff eh ef
runReader r = runAsk r . runLocal

-- | Elaborate the t'Local' effect.
runLocal
    :: forall r eh ef
     . (Ask r <| ef)
    => Eff (Local r ': eh) ef ~> Eff eh ef
runLocal = interpretH elabLocal

elabLocal
    :: forall r eh ef
     . (Ask r <| ef)
    => Local r ~~> Eff eh ef
elabLocal (Local f a) = a & interpose @(Ask r) \Ask -> f <$> ask

-- | Interpret the t'Ask' effect.
runAsk
    :: forall r ef eh
     . r
    -> Eff eh (Ask r ': ef) ~> Eff eh ef
runAsk r = interpret \Ask -> pure r
