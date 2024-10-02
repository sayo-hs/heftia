{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

module Control.Monad.Hefty.Transform where

import Control.Effect (type (~>))
import Control.Monad.Hefty.Interpret (iterAllEffHFBy)
import Control.Monad.Hefty.Types (Eff, sendUnionBy, sendUnionHBy)
import Data.Effect.HFunctor (HFunctor)
import Data.Effect.OpenUnion.Internal (
    BundleUnder,
    Drop,
    IsSuffixOf,
    Split,
    Strengthen,
    StrengthenN,
    StrengthenNUnder,
    StrengthenUnder,
    Take,
    WeakenN,
    WeakenNUnder,
 )
import Data.Effect.OpenUnion.Internal.FO (
    Union,
    bundleAllUnion,
    bundleUnion,
    bundleUnionN,
    bundleUnionUnder,
    decomp,
    inj,
    prj,
    strengthenN,
    strengthenNUnder,
    strengthens,
    strengthensUnder,
    unbundleAllUnion,
    unbundleUnion,
    unbundleUnionN,
    unbundleUnionUnder,
    weakenN,
    weakenNUnder,
    weakens,
    type (<!),
 )
import Data.Effect.OpenUnion.Internal.HO (
    UnionH,
    bundleAllUnionH,
    bundleUnionH,
    bundleUnionUnderH,
    decompH,
    hfmapUnion,
    injH,
    prjH,
    strengthenNH,
    strengthenNUnderH,
    strengthensH,
    unbundleAllUnionH,
    unbundleUnionH,
    unbundleUnionUnderH,
    weakenH,
    weakenNH,
    weakenNUnderH,
    weakensH,
    type (<!!),
 )
import GHC.TypeNats (KnownNat)

transform :: forall e e' ef eh. (e ~> e') -> Eff eh (e ': ef) ~> Eff eh (e' ': ef)
transform f = transEff (either id (inj . f) . decomp)
{-# INLINE transform #-}

transformH
    :: forall e e' eh ef
     . (HFunctor e)
    => (e (Eff (e' ': eh) ef) ~> e' (Eff (e' ': eh) ef))
    -> Eff (e ': eh) ef ~> Eff (e' ': eh) ef
transformH f = transEffH (either weakenH (injH . f) . decompH)
{-# INLINE transformH #-}

translate :: forall e e' ef eh. (e' <! ef) => (e ~> e') -> Eff eh (e ': ef) ~> Eff eh ef
translate f = transEff (either id (inj . f) . decomp)
{-# INLINE translate #-}

translateH
    :: forall e e' eh ef
     . (e' <!! eh, HFunctor e)
    => (e (Eff eh ef) ~> e' (Eff eh ef))
    -> Eff (e ': eh) ef ~> Eff eh ef
translateH f = transEffH (either id (injH . f) . decompH)
{-# INLINE translateH #-}

rewrite :: forall e ef eh. (e <! ef) => (e ~> e) -> Eff eh ef ~> Eff eh ef
rewrite f = transEff \u -> maybe u (inj . f) $ prj @e u
{-# INLINE rewrite #-}

rewriteH
    :: forall e eh ef
     . (e <!! eh, HFunctor e)
    => (e (Eff eh ef) ~> e (Eff eh ef))
    -> Eff eh ef ~> Eff eh ef
rewriteH f = transEffH \u -> maybe u (injH . f) $ prjH @e u
{-# INLINE rewriteH #-}

raises :: (ef `IsSuffixOf` ef') => Eff eh ef ~> Eff eh ef'
raises = transEff weakens
{-# INLINE raises #-}

raiseN :: forall len ef ef' eh. (WeakenN len ef ef') => Eff eh ef ~> Eff eh ef'
raiseN = transEff (weakenN @len)
{-# INLINE raiseN #-}

raiseNUnder
    :: forall len offset ef ef' eh
     . (WeakenNUnder len offset ef ef')
    => Eff eh ef ~> Eff eh ef'
raiseNUnder = transEff (weakenNUnder @len @offset)
{-# INLINE raiseNUnder #-}

raisesH :: (eh `IsSuffixOf` eh') => Eff eh ef ~> Eff eh' ef
raisesH = transEffH weakensH
{-# INLINE raisesH #-}

raiseNH :: forall len eh eh' ef. (WeakenN len eh eh') => Eff eh ef ~> Eff eh' ef
raiseNH = transEffH (weakenNH @len)
{-# INLINE raiseNH #-}

raiseNUnderH
    :: forall len offset eh eh' ef
     . (WeakenNUnder len offset eh eh')
    => Eff eh ef ~> Eff eh' ef
raiseNUnderH = transEffH (weakenNUnderH @len @offset)
{-# INLINE raiseNUnderH #-}

subsumes :: forall ef ef' eh. (Strengthen ef ef') => Eff eh ef ~> Eff eh ef'
subsumes = transEff strengthens
{-# INLINE subsumes #-}

subsumeN :: forall len ef ef' eh. (StrengthenN len ef ef') => Eff eh ef ~> Eff eh ef'
subsumeN = transEff (strengthenN @len)
{-# INLINE subsumeN #-}

subsumesUnder
    :: forall offset ef ef' eh
     . (StrengthenUnder offset ef ef')
    => Eff eh ef ~> Eff eh ef'
subsumesUnder = transEff (strengthensUnder @offset)
{-# INLINE subsumesUnder #-}

subsumeNUnder
    :: forall len offset ef ef' eh
     . (StrengthenNUnder len offset ef ef')
    => Eff eh ef ~> Eff eh ef'
subsumeNUnder = transEff (strengthenNUnder @len @offset)
{-# INLINE subsumeNUnder #-}

subsumesH :: forall eh eh' ef. (Strengthen eh eh') => Eff eh ef ~> Eff eh' ef
subsumesH = transEffH strengthensH
{-# INLINE subsumesH #-}

subsumeNH :: forall len eh eh' ef. (StrengthenN len eh eh') => Eff eh ef ~> Eff eh' ef
subsumeNH = transEffH (strengthenNH @len)
{-# INLINE subsumeNH #-}

subsumeNUnderH
    :: forall len offset eh eh' ef
     . (StrengthenNUnder len offset eh eh')
    => Eff eh ef ~> Eff eh' ef
subsumeNUnderH = transEffH (strengthenNUnderH @len @offset)
{-# INLINE subsumeNUnderH #-}

-- TODO: add raiseUnder(H), subsume(H), subsumeUnder(H)

bundle
    :: forall ef bundle rest eh
     . (Split ef bundle rest)
    => Eff eh ef ~> Eff eh (Union bundle ': rest)
bundle = transEff bundleUnion
{-# INLINE bundle #-}

bundleN
    :: forall len ef eh
     . (KnownNat len)
    => Eff eh ef ~> Eff eh (Union (Take len ef) ': Drop len ef)
bundleN = transEff (bundleUnionN @len)
{-# INLINE bundleN #-}

unbundle
    :: forall ef bundle rest eh
     . (Split ef bundle rest)
    => Eff eh (Union bundle ': rest) ~> Eff eh ef
unbundle = transEff unbundleUnion
{-# INLINE unbundle #-}

unbundleN
    :: forall len ef eh
     . (KnownNat len)
    => Eff eh (Union (Take len ef) ': Drop len ef) ~> Eff eh ef
unbundleN = transEff (unbundleUnionN @len)
{-# INLINE unbundleN #-}

bundleUnder
    :: forall offset bundle ef ef' eh
     . (BundleUnder Union offset ef ef' bundle)
    => Eff eh ef ~> Eff eh ef'
bundleUnder = transEff (bundleUnionUnder @offset)
{-# INLINE bundleUnder #-}

-- TODO: add *bundle*N(H) functions

unbundleUnder
    :: forall offset bundle ef ef' eh
     . (BundleUnder Union offset ef ef' bundle)
    => Eff eh ef' ~> Eff eh ef
unbundleUnder = transEff (unbundleUnionUnder @offset)
{-# INLINE unbundleUnder #-}

bundleAll :: Eff eh ef ~> Eff eh '[Union ef]
bundleAll = transEff bundleAllUnion
{-# INLINE bundleAll #-}

unbundleAll :: Eff eh '[Union ef] ~> Eff eh ef
unbundleAll = transEff unbundleAllUnion
{-# INLINE unbundleAll #-}

bundleH
    :: forall eh bundle rest ef
     . (Split eh bundle rest)
    => Eff eh ef ~> Eff (UnionH bundle ': rest) ef
bundleH = transEffH bundleUnionH
{-# INLINE bundleH #-}

unbundleH
    :: forall eh bundle rest ef
     . (Split eh bundle rest)
    => Eff (UnionH bundle ': rest) ef ~> Eff eh ef
unbundleH = transEffH unbundleUnionH
{-# INLINE unbundleH #-}

bundleUnderH
    :: forall offset bundle eh eh' ef
     . (BundleUnder UnionH offset eh eh' bundle)
    => Eff eh ef ~> Eff eh' ef
bundleUnderH = transEffH (bundleUnionUnderH @offset)
{-# INLINE bundleUnderH #-}

unbundleUnderH
    :: forall offset bundle eh eh' ef
     . (BundleUnder UnionH offset eh eh' bundle)
    => Eff eh' ef ~> Eff eh ef
unbundleUnderH = transEffH (unbundleUnionUnderH @offset)
{-# INLINE unbundleUnderH #-}

bundleAllH :: Eff eh ef ~> Eff '[UnionH eh] ef
bundleAllH = transEffH bundleAllUnionH
{-# INLINE bundleAllH #-}

unbundleAllH :: Eff '[UnionH eh] ef ~> Eff eh ef
unbundleAllH = transEffH unbundleAllUnionH
{-# INLINE unbundleAllH #-}

transEff
    :: forall ef ef' eh
     . (Union ef ~> Union ef')
    -> Eff eh ef ~> Eff eh ef'
transEff = transEffHF id
{-# INLINE transEff #-}

transEffH
    :: forall eh eh' ef
     . (UnionH eh (Eff eh' ef) ~> UnionH eh' (Eff eh' ef))
    -> Eff eh ef ~> Eff eh' ef
transEffH f = transEffHF f id
{-# INLINE transEffH #-}

transEffHF
    :: forall eh eh' ef ef'
     . (UnionH eh (Eff eh' ef') ~> UnionH eh' (Eff eh' ef'))
    -> (Union ef ~> Union ef')
    -> Eff eh ef ~> Eff eh' ef'
transEffHF fh ff = loop
  where
    loop :: Eff eh ef ~> Eff eh' ef'
    loop =
        iterAllEffHFBy
            pure
            (flip sendUnionHBy . fh . hfmapUnion loop)
            (flip sendUnionBy . ff)
{-# INLINE transEffHF #-}
