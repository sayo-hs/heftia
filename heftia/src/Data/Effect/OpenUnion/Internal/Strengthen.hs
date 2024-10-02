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
import Data.Type.Equality (type (==))
import GHC.TypeLits (KnownNat, Natural, type (-))

type Strengthen len = StrengthenUnder_ 'True 0 (len == 0) len
type StrengthenUnder len offset = StrengthenUnder_ (offset == 0) offset (len == 0) len

strengthenUnderMap :: forall len offset es es'. (StrengthenUnder len offset es es') => Word -> Word
strengthenUnderMap = strengthenUnderMap_ @_ @(offset == 0) @offset @(len == 0) @len @es @es'

class
    ( isOffsetZero ~ (offset == 0)
    , isLenZero ~ (len == 0)
    , KnownNat offset
    , KnownNat len
    ) =>
    StrengthenUnder_ (isOffsetZero :: Bool) (offset :: Natural) (isLenZero :: Bool) (len :: Natural) (es :: [k]) (es' :: [k])
        | offset len es -> es'
    where
    strengthenUnderMap_ :: Word -> Word

instance
    ( StrengthenUnder len (offset - 1) es es'
    , (offset == 0) ~ 'False
    , isLenZero ~ (len == 0)
    , KnownNat offset
    , KnownNat len
    )
    => StrengthenUnder_ 'False offset isLenZero len (e ': es) (e ': es')
    where
    strengthenUnderMap_ = strengthenUnderMap @len @(offset - 1) @es @es'

instance
    (Strengthen (len - 1) es es', FindElem e es, (len == 0) ~ 'False, KnownNat len)
    => StrengthenUnder_ 'True 0 'False len (e ': es) es'
    where
    strengthenUnderMap_ = \case
        0 -> unP $ elemNo @_ @e @es
        n -> strengthenUnderMap @(len - 1) @0 @es @es' $ n - 1
    {-# INLINE strengthenUnderMap_ #-}

instance StrengthenUnder_ 'True 0 'True 0 es es where
    strengthenUnderMap_ = id
    {-# INLINE strengthenUnderMap_ #-}
