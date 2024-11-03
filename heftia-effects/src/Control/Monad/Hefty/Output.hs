-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
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
import Control.Monad.Hefty (Eff, interpret, interpretStateBy, raiseUnder, type (~>))
import Control.Monad.Hefty.State (runState)
import Control.Monad.Hefty.Writer (handleTell)
import Data.Effect.Output
import Data.Effect.State (modify)
import Data.Effect.Writer (Tell (Tell))

-- | Interprets the t'Output' effect using the given output handler.
runOutputEff
    :: forall o ef eh
     . (o -> Eff eh ef ())
    -> Eff eh (Output o ': ef) ~> Eff eh ef
runOutputEff f = interpret \(Output o) -> f o

-- | Interprets the t'Output' effect by ignoring the outputs.
ignoreOutput
    :: forall o ef eh
     . Eff eh (Output o ': ef) ~> Eff eh ef
ignoreOutput = runOutputEff $ const $ pure ()

-- | Interprets the t'Output' effect by accumulating the outputs into a list.
runOutputList
    :: forall o a ef
     . Eff '[] (Output o ': ef) a
    -> Eff '[] ef ([o], a)
runOutputList =
    raiseUnder
        >>> interpret (\(Output o) -> modify (o :))
        >>> runState []

-- | Interprets the t'Output' effect by accumulating the outputs into a monoid.
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
