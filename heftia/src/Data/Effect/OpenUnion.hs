-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp
-}
module Data.Effect.OpenUnion (
    module Data.Effect.OpenUnion.Internal,
    module Data.Effect.OpenUnion.Internal.HO,
    module Data.Effect.OpenUnion.Internal.FO,
    module Data.Effect.OpenUnion.Sum,
) where

import Data.Effect.OpenUnion.Internal (
    BundleUnder,
    Drop,
    ElemAt,
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
    WeakenUnder,
    type (++),
 )
import Data.Effect.OpenUnion.Internal.FO (
    Lookup,
    Member,
    MemberBy,
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
    inj0,
    injN,
    nil,
    prefixUnion,
    prefixUnionUnder,
    prj,
    prjN,
    strengthen,
    strengthenN,
    strengthenNUnder,
    strengthenUnder,
    suffixUnion,
    suffixUnionOverN,
    unbundleAllUnion,
    unbundleUnion,
    unbundleUnionUnder,
    weaken,
    weakenN,
    weakenNUnder,
    weakenUnder,
    weakens,
    weakensUnder,
    (!+),
    type (<|),
 )
import Data.Effect.OpenUnion.Internal.HO (
    HFunctors,
    IsHFunctors,
    LookupH,
    MemberH,
    MemberHBy,
    NotHFunctor,
    UnionH,
    bundleAllUnionH,
    bundleUnionH,
    bundleUnionUnderH,
    decomp0H,
    decomp0H_,
    decompH,
    decompH_,
    extractH,
    extractH_,
    flipAllUnionH,
    flipUnionH,
    flipUnionUnderH,
    hfmapUnion,
    inj0H,
    injH,
    injNH,
    nilH,
    prefixUnionH,
    prefixUnionUnderH,
    prjH,
    prjH_,
    prjNH,
    prjNH_,
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
    weakenUnderH,
    weakensH,
    weakensUnderH,
    (!!+),
    (!!+.),
    ($+),
    type (<<|),
 )
import Data.Effect.OpenUnion.Sum (
    SumToRecUnion,
    SumToRecUnionList,
    U,
    UL,
    type (+),
 )

-- TODO: add move/swap/insert/rotate functions.
