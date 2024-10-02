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
module Control.Effect.Interpreter.Heftia.Output where

import Control.Arrow ((>>>))
import Control.Effect (type (~>))
import Control.Effect.Interpreter.Heftia.State (runState)
import Control.Effect.Interpreter.Heftia.Writer (handleTell)
import Control.Monad.Hefty (
    Eff,
    HFunctors,
    interpret,
    interpretRec,
    interpretStateBy,
    raiseUnder,
 )
import Data.Effect.Output (Output (Output))
import Data.Effect.State (modify)
import Data.Effect.Writer (Tell (Tell))

runOutputEff
    :: forall o ef eh
     . (HFunctors eh)
    => (o -> Eff eh ef ())
    -> Eff eh (Output o ': ef) ~> Eff eh ef
runOutputEff f = interpretRec \(Output o) -> f o

ignoreOutput
    :: forall o ef eh
     . (HFunctors eh)
    => Eff eh (Output o ': ef) ~> Eff eh ef
ignoreOutput = runOutputEff $ const $ pure ()

runOutputList
    :: forall o a ef
     . Eff '[] (Output o ': ef) a
    -> Eff '[] ef ([o], a)
runOutputList =
    raiseUnder
        >>> interpret (\(Output o) -> modify (o :))
        >>> runState []

-- | Run an `Output` effect by transforming into a monoid.
runOutputMonoid
    :: forall o w a ef
     . ( Monoid w
       )
    => (o -> w)
    -> Eff '[] (Output o ': ef) a
    -> Eff '[] ef (w, a)
runOutputMonoid f =
    interpretStateBy mempty (curry pure) \(Output o) ->
        handleTell $ Tell $ f o
