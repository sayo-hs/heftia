-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

Interpreters for the t'Output' effect.
-}
module Control.Monad.Hefty.Output (
    module Control.Monad.Hefty.Output,
    module Data.Effect.Output,
)
where

import Control.Arrow ((>>>))
import Control.Monad.Hefty (Eff, FOEs, interpret, interpretStateBy, raiseUnder)
import Control.Monad.Hefty.State (runState)
import Control.Monad.Hefty.Writer (handleTell)
import Data.Effect.Output
import Data.Effect.State (modify)
import Data.Effect.Writer (Tell (Tell))

-- | Interprets the t'Output' effect by accumulating the outputs into a list.
runOutputList
    :: forall o a es
     . (FOEs es)
    => Eff (Output o ': es) a
    -> Eff es ([o], a)
runOutputList =
    raiseUnder
        >>> interpret (\(Output o) -> modify (o :))
        >>> runState []

-- | Interprets the t'Output' effect by accumulating the outputs into a monoid.
runOutputMonoid
    :: forall o w a es
     . (Monoid w, FOEs es)
    => (o -> w)
    -> Eff (Output o ': es) a
    -> Eff es (w, a)
runOutputMonoid f =
    interpretStateBy mempty (curry pure) \(Output o) ->
        handleTell $ Tell $ f o
