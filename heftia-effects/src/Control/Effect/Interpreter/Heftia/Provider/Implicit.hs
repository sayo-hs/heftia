-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

Elaborator for the t'Control.Effect.Class.Provider.Implicit.ImplicitProvider' effect class.
-}
module Control.Effect.Interpreter.Heftia.Provider.Implicit where

import Control.Effect (type (~>))
import Control.Effect.Hefty (Eff, Elab, raise)
import Control.Effect.Interpreter.Heftia.Reader (runAsk)
import Control.Freer (Freer)
import Data.Effect.HFunctor (HFunctor)
import Data.Effect.Provider.Implicit (ImplicitProvider' (WithImplicit))
import Data.Effect.Reader (LAsk)
import Data.Hefty.Union (Union)

-- | Elaborate the t'ImplicitProvider'' effect using the given interpreter.
elaborateImplicitProvider ::
    (c g, e g) =>
    (f ~> g) ->
    (i -> forall x. g x -> f x) ->
    Elab (ImplicitProvider' c i e) f
elaborateImplicitProvider iLower run (WithImplicit i f) = run i $ f iLower
{-# INLINE elaborateImplicitProvider #-}

runImplicitProvider ::
    ( e (Eff u fr eh (LAsk i ': ef))
    , c (Eff u fr eh (LAsk i ': ef))
    , Freer c fr
    , Union u
    , HFunctor (u eh)
    , Applicative (Eff u fr eh ef)
    ) =>
    Elab (ImplicitProvider' c i e) (Eff u fr eh ef)
runImplicitProvider (WithImplicit i f) = runAsk i $ f raise
{-# INLINE runImplicitProvider #-}
