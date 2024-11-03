-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2023 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Interpreters for the [Reader]("Data.Effect.Reader") effects.
-}
module Control.Monad.Hefty.Reader (
    module Control.Monad.Hefty.Reader,
    module Data.Effect.Reader,
)
where

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
import Data.Effect.Reader

-- | Interpret the t'Ask'/t'Local' effects.
runReader
    :: forall r eh ef
     . r
    -> Eff (Local r ': eh) (Ask r ': ef) ~> Eff eh ef
runReader r = runAsk r . runLocal

-- | Interpret the t'Local' effect.
runLocal
    :: forall r eh ef
     . (Ask r <| ef)
    => Eff (Local r ': eh) ef ~> Eff eh ef
runLocal = interpretH elabLocal

-- | A elaborator function for the t'Local' effect.
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
