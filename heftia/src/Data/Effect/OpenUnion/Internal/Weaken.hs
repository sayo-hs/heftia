{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
Portability :  portable
-}
module Data.Effect.OpenUnion.Internal.Weaken where

import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat, natVal, type (-))

class Weaken len es es' where
    weakenLen :: Word

instance Weaken 0 es es where
    weakenLen = 0

instance {-# OVERLAPPABLE #-} (Weaken (len - 1) es es', KnownNat len) => Weaken len es (e ': es') where
    weakenLen = fromIntegral $ natVal @len Proxy

class WeakenUnder len offset es es' where
    weakenUnderLen :: Word
    weakenUnderOffset :: Word

instance (Weaken len es es') => WeakenUnder len 0 es es' where
    weakenUnderLen = weakenLen @len @es @es'
    weakenUnderOffset = 0

instance {-# OVERLAPPABLE #-} (WeakenUnder len (offset - 1) es es', KnownNat offset) => WeakenUnder len offset es (e ': es') where
    weakenUnderLen = weakenUnderLen @len @(offset - 1) @es @es'
    weakenUnderOffset = fromIntegral $ natVal @offset Proxy
