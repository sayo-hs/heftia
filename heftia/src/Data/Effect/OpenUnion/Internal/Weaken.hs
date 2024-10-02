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

import Data.Type.Equality (type (==))
import GHC.TypeNats (KnownNat, Natural, type (-))

type Weaken len = WeakenUnder_ 'True 0 (len == 0) len
type WeakenUnder len offset = WeakenUnder_ (offset == 0) offset (len == 0) len

class
    (isOffsetZero ~ (offset == 0), isLenZero ~ (len == 0), KnownNat offset, KnownNat len) =>
    WeakenUnder_ (isOffsetZero :: Bool) (offset :: Natural) (isLenZero :: Bool) (len :: Natural) es es'
        | es' -> es

instance
    ( WeakenUnder n (offset - 1) es es'
    , (offset == 0) ~ 'False
    , isLenZero ~ (len == 0)
    , KnownNat offset
    , KnownNat len
    )
    => WeakenUnder_ 'False offset isLenZero len (e ': es) (e ': es')

instance
    (Weaken (len - 1) es es', (len == 0) ~ 'False, KnownNat len)
    => WeakenUnder_ 'True 0 'False len es (e ': es')

instance WeakenUnder_ 'True 0 'True 0 es es
