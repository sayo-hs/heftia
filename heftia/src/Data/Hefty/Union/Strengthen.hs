{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{- |
Copyright   :  (c) 2024 Yamada Ryo
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Data.Hefty.Union.Strengthen where

import Control.Effect (type (~>))
import Data.Hefty.Union (MemberRec, Union (inject0, (|+:)), injectRec, weaken)
import Data.Type.Equality (type (==))
import GHC.TypeNats (Natural, type (-))

class
    (Union u, isMZero ~ (m == 0), isNZero ~ (n == 0)) =>
    StrengthenUnder_ (isMZero :: Bool) (m :: Natural) (isNZero :: Bool) (n :: Natural) u es es'
        | m n es -> es'
    where
    strengthenNUnderM_ :: u es f ~> u es' f

type StrengthenUnder n m = StrengthenUnder_ (m == 0) m (n == 0) n
type Strengthen n = StrengthenUnder_ 'True 0 (n == 0) n

strengthenNUnderM :: forall n m u es' f es. StrengthenUnder n m u es es' => u es f ~> u es' f
strengthenNUnderM = strengthenNUnderM_ @(m == 0) @m @(n == 0) @n
{-# INLINE strengthenNUnderM #-}

strengthenN :: forall n u es' f es. Strengthen n u es es' => u es f ~> u es' f
strengthenN = strengthenNUnderM_ @( 'True) @0 @(n == 0) @n
{-# INLINE strengthenN #-}

instance
    (Union u, StrengthenUnder n (m - 1) u es es', (m == 0) ~ 'False, isNZero ~ (n == 0)) =>
    StrengthenUnder_ 'False m isNZero n u (e ': es) (e ': es')
    where
    strengthenNUnderM_ = inject0 |+: weaken . strengthenNUnderM @n @(m - 1)
    {-# INLINE strengthenNUnderM_ #-}

instance
    (Union u, Strengthen (n - 1) u es es', MemberRec u e es, (n == 0) ~ 'False) =>
    StrengthenUnder_ 'True 0 'False n u (e ': es) es'
    where
    strengthenNUnderM_ = strengthenN @(n - 1) . (injectRec |+: id)
    {-# INLINE strengthenNUnderM_ #-}

instance Union u => StrengthenUnder_ 'True 0 'True 0 u es es where
    strengthenNUnderM_ = id
    {-# INLINE strengthenNUnderM_ #-}
