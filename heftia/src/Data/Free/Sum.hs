-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2023-2024 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable

Binary sums for first-order effects.
-}
module Data.Free.Sum (module Data.Free.Sum, pattern L1, pattern R1) where

import Control.Effect (type (~>))
import Data.Effect (Nop)
import GHC.Generics (type (:+:) (L1, R1))

type (+) = (:+:)
infixr 5 +

caseF :: (f a -> r) -> (g a -> r) -> (f + g) a -> r
caseF f g = \case
    L1 x -> f x
    R1 x -> g x
{-# INLINE caseF #-}

absurdL :: Nop + f ~> f
absurdL = caseF \case {} id
{-# INLINE absurdL #-}

absurdR :: f + Nop ~> f
absurdR = caseF id \case {}
{-# INLINE absurdR #-}

swapSum :: (f + g) a -> (g + f) a
swapSum = caseF R1 L1
{-# INLINE swapSum #-}
