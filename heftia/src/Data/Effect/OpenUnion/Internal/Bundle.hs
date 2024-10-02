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
module Data.Effect.OpenUnion.Internal.Bundle where

import Data.Effect.OpenUnion.Internal (KnownLength)
import GHC.TypeLits (KnownNat, Natural, type (-))

class
    (KnownLength bundle) =>
    Bundle (es :: [k]) (bundle :: [k]) (rest :: [k])
        | bundle es -> rest
        , bundle rest -> es

instance Bundle es '[] es

instance
    (Bundle es bundle rest, KnownLength (e ': bundle))
    => Bundle (e ': es) (e ': bundle) rest

class
    (KnownNat offset, KnownLength bundle) =>
    BundleUnder (u :: [k] -> k) (offset :: Natural) (es :: [k]) (es' :: [k]) bundle
        | offset es es' -> bundle
        , u offset es bundle -> es'

instance (Bundle es bundle rest) => BundleUnder u 0 es (u bundle ': rest) bundle

instance
    {-# OVERLAPPABLE #-}
    (BundleUnder u (offset - 1) es es' bundle, KnownNat offset, KnownLength bundle)
    => BundleUnder u offset (e ': es) (e ': es') bundle
