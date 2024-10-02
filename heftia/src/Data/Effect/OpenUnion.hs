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
) where

import Data.Effect.OpenUnion.Internal (
    BundleUnder,
    Drop,
    IsSuffixOf,
    KnownLength,
    Length,
    Reverse,
    Split,
    Strengthen,
    StrengthenN,
    StrengthenNUnder,
    StrengthenUnder,
    Take,
    WeakenN,
    WeakenNUnder,
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
    strengthenNUnder,
    suffixUnion,
    suffixUnionOverN,
    unbundleAllUnion,
    unbundleUnion,
    unbundleUnionUnder,
    weaken,
    weakenN,
    weakenNUnder,
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
    strengthenNUnderH,
    suffixUnionH,
    suffixUnionOverNH,
    unbundleAllUnionH,
    unbundleUnionH,
    unbundleUnionUnderH,
    weakenH,
    weakenNH,
    weakenNUnderH,
    weakensH,
    (!!+),
    type (<!!),
 )

-- TODO: add injN/prjN/move/swap/insert/rotate functions.
