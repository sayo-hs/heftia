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
module Data.Effect.OpenUnion.Internal.Strengthen where

import Data.Effect.OpenUnion.Internal (FindElem (elemNo), P (unP))
import Data.Proxy (Proxy (Proxy))
import GHC.TypeLits (KnownNat, natVal, type (-))

class Strengthen len es es' where
    strengthenMap :: Word -> Word

instance Strengthen 0 es es where
    strengthenMap = id
    {-# INLINE strengthenMap #-}

instance {-# OVERLAPPABLE #-} (Strengthen (len - 1) es es', FindElem e es) => Strengthen len (e ': es) es' where
    strengthenMap = \case
        0 -> unP $ elemNo @_ @e @es
        n -> strengthenMap @(len - 1) @es @es' $ n - 1
    {-# INLINE strengthenMap #-}

class StrengthenUnder len offset es es' where
    strengthenUnderMap :: Word -> Word
    strengthenUnderOffset :: Word

instance (Strengthen len es es') => StrengthenUnder len 0 es es' where
    strengthenUnderMap = strengthenMap @len @es @es'
    strengthenUnderOffset = 0

instance {-# OVERLAPPABLE #-} (StrengthenUnder len (offset - 1) es es', KnownNat offset) => StrengthenUnder len offset es (e ': es') where
    strengthenUnderMap = strengthenUnderMap @len @(offset - 1) @es @es'
    strengthenUnderOffset = fromIntegral $ natVal @offset Proxy
