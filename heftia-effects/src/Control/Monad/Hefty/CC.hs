-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024-2025 Sayo contributors
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
-}
module Control.Monad.Hefty.CC (
    module Control.Monad.Hefty.CC,
    module Data.Effect.CC,
)
where

import Control.Monad.Hefty (AlgHandler, Eff, interpretBy)
import Data.Effect.CC
import Data.Effect.OpenUnion (FOEs)
import Data.Functor.Contravariant (Op (Op))

runCC :: (FOEs es) => (a -> Eff es ans) -> Eff (CC (Op (Eff es ans)) ': es) a -> Eff es ans
runCC k = interpretBy k handleCC
{-# INLINE runCC #-}

handleCC :: AlgHandler (CC (Op (g ans))) f g ans
handleCC = \case
    SubFork -> \exit -> exit . Left . Op $ exit . Right
    Jump (Op exit) x -> \_ -> exit x
{-# INLINE handleCC #-}

evalCC :: (FOEs es) => Eff (CC (Op (Eff es a)) ': es) a -> Eff es a
evalCC = runCC pure
{-# INLINE evalCC #-}
