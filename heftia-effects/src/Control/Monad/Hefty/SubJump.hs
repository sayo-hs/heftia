{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
-}
module Control.Monad.Hefty.SubJump (
    module Control.Monad.Hefty.SubJump,
    module Data.Effect.SubJump,
)
where

import Control.Arrow ((>>>))
import Control.Monad.Hefty (Eff, interpretBy, unkey)
import Data.Effect.SubJump
import Data.Functor.Contravariant qualified as C

runSubJump :: (a -> Eff '[] ef ans) -> Eff '[] (SubJump (C.Op (Eff '[] ef ans)) ': ef) a -> Eff '[] ef ans
runSubJump k =
    unkey >>> interpretBy k \case
        SubFork -> \exit -> exit . Left . C.Op $ exit . Right
        Jump (C.Op exit) x -> \_ -> exit x

evalSubJump :: Eff '[] (SubJump (C.Op (Eff '[] ef a)) ': ef) a -> Eff '[] ef a
evalSubJump = runSubJump pure
