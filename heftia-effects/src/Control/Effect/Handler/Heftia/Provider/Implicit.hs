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
module Control.Effect.Handler.Heftia.Provider.Implicit where

import Control.Effect.Class (type (~>))
import Control.Effect.Class.Provider.Implicit (ImplicitProviderS (WithImplicit))
import Control.Effect.Class.Reader (AskI)
import Control.Effect.Freer (Fre, raise)
import Control.Effect.Handler.Heftia.Reader (interpretAsk)
import Control.Effect.Heftia (Elaborator)

{- |
Elaborate the t'Control.Effect.Class.Provider.Implicit.ImplicitProvider' effect using the given
interpreter.
-}
elaborateImplicitProvider ::
    (c g, e g) =>
    (f ~> g) ->
    (i -> forall x. g x -> f x) ->
    Elaborator (ImplicitProviderS c i e) f
elaborateImplicitProvider iLower run (WithImplicit i f) = run i $ f iLower
{-# INLINE elaborateImplicitProvider #-}

-- todo: make the 'classy-effects-static' handler system and use the static Reader carrier here.
runImplicitProvider ::
    (c (Fre (AskI i ': r) m), e (Fre (AskI i ': r) m), Monad m) =>
    Elaborator (ImplicitProviderS c i e) (Fre r m)
runImplicitProvider (WithImplicit i f) = interpretAsk i $ f raise
