-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
Stability   :  experimental
-}
module Data.Effect.OpenUnion (
    module Data.Effect.OpenUnion.Internal,
    module Data.Effect.OpenUnion.Internal.HO,
    module Data.Effect.OpenUnion.Internal.FO,
    module Data.Effect.OpenUnion.Internal.Strengthen,
    module Data.Effect.OpenUnion.Internal.Weaken,
) where

import Data.Effect.OpenUnion.Internal (
    Drop,
    KnownLength,
    Length,
    Reverse,
    Take,
    type (++),
 )
import Data.Effect.OpenUnion.Internal.FO (
    EffectF,
    Member,
    Union,
    bundleAllUnion,
    bundleUnion,
    bundleUnionUnder,
    decomp,
    decomp0,
    extract,
    flipAllUnion,
    flipUnion,
    flipUnionUnder,
    inj,
    nil,
    prefixUnion,
    prefixUnionUnder,
    prj,
    strengthenN,
    strengthenNUnderM,
    suffixUnion,
    suffixUnionOverN,
    unbundleAllUnion,
    unbundleUnion,
    unbundleUnionUnder,
    weaken,
    weakenN,
    weakenNUnderM,
    weakens,
    (!+),
    type (<!),
 )
import Data.Effect.OpenUnion.Internal.HO (
    EffectH,
    MemberH,
    UnionH,
    bundleAllUnionH,
    bundleUnionH,
    bundleUnionUnderH,
    decomp0H,
    decompH,
    extractH,
    flipAllUnionH,
    flipUnionH,
    flipUnionUnderH,
    hfmapUnion,
    injH,
    nilH,
    prefixUnionH,
    prefixUnionUnderH,
    prjH,
    strengthenNH,
    strengthenNUnderMH,
    suffixUnionH,
    suffixUnionOverNH,
    unbundleAllUnionH,
    unbundleUnionH,
    unbundleUnionUnderH,
    weakenH,
    weakenNH,
    weakenNUnderMH,
    weakensH,
    (!!+),
    type (<!!),
 )
import Data.Effect.OpenUnion.Internal.Strengthen (Strengthen, StrengthenUnder)
import Data.Effect.OpenUnion.Internal.Weaken (IsSuffixOf, Weaken, WeakenUnder)
