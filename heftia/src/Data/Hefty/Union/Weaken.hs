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
module Data.Hefty.Union.Weaken where

import Control.Effect (type (~>))
import Data.Hefty.Union (Union (inject0, weaken, (|+:)))
import Data.Type.Equality (type (==))
import GHC.TypeNats (Natural, type (-))

class
    (isMZero ~ (m == 0), isNZero ~ (n == 0)) =>
    WeakenUnder_ (isMZero :: Bool) (m :: Natural) (isNZero :: Bool) (n :: Natural) es es'
        | m n es' -> es
    where
    weakenNUnderM_ :: Union u => u es f ~> u es' f

type WeakenUnder n m = WeakenUnder_ (m == 0) m (n == 0) n
type Weaken n = WeakenUnder_ 'True 0 (n == 0) n

weakenNUnderM :: forall n m u es' f es. (Union u, WeakenUnder n m es es') => u es f ~> u es' f
weakenNUnderM = weakenNUnderM_ @(m == 0) @m @(n == 0) @n
{-# INLINE weakenNUnderM #-}

weakenN :: forall n u es' f es. (Union u, Weaken n es es') => u es f ~> u es' f
weakenN = weakenNUnderM_ @( 'True) @0 @(n == 0) @n
{-# INLINE weakenN #-}

instance
    (WeakenUnder n (m - 1) es es', (m == 0) ~ 'False, isNZero ~ (n == 0)) =>
    WeakenUnder_ 'False m isNZero n (e ': es) (e ': es')
    where
    weakenNUnderM_ = inject0 |+: weaken . weakenNUnderM @n @(m - 1)
    {-# INLINE weakenNUnderM_ #-}

instance
    (Weaken (n - 1) es es', (n == 0) ~ 'False) =>
    WeakenUnder_ 'True 0 'False n es (e ': es')
    where
    weakenNUnderM_ = weaken . weakenN @(n - 1)
    {-# INLINE weakenNUnderM_ #-}

instance WeakenUnder_ 'True 0 'True 0 es es where
    weakenNUnderM_ = id
    {-# INLINE weakenNUnderM_ #-}
