-- SPDX-License-Identifier: MPL-2.0
{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Monad.Hefty.Transform where

import Control.Effect (type (~>))
import Control.Monad.Hefty.Interpret (interpret, iterAllEffHFBy, runPure)
import Control.Monad.Hefty.Types (Eff, sendUnionBy, sendUnionHBy)
import Data.Effect.HFunctor (HFunctor)
import Data.Effect.OpenUnion.Internal.Bundle (Bundle, BundleUnder)
import Data.Effect.OpenUnion.Internal.FO (
    Union,
    bundleAllUnion,
    bundleUnion,
    bundleUnionUnder,
    decomp,
    inj,
    prj,
    strengthenN,
    strengthenNUnderM,
    unbundleAllUnion,
    unbundleUnion,
    unbundleUnionUnder,
    weakenN,
    weakenNUnderM,
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
    strengthenNUnderMH,
    unbundleAllUnionH,
    unbundleUnionH,
    unbundleUnionUnderH,
    weakenH,
    weakenNH,
    weakenNUnderMH,
    weakensH,
    type (<!!),
 )
import Data.Effect.OpenUnion.Internal.Strengthen (Strengthen, StrengthenUnder)
import Data.Effect.OpenUnion.Internal.Weaken (IsSuffixOf, Weaken, WeakenUnder)
import Data.Effect.State (State)

transform :: forall e e' ef eh. (e ~> e') -> Eff eh (e ': ef) ~> Eff eh (e' ': ef)
transform f = transEffHF id (either id (inj . f) . decomp)
{-# INLINE transform #-}

transformH
    :: forall e e' eh ef
     . (HFunctor e)
    => (e (Eff (e' ': eh) ef) ~> e' (Eff (e' ': eh) ef))
    -> Eff (e ': eh) ef ~> Eff (e' ': eh) ef
transformH f = transEffHF (either weakenH (injH . f) . decompH) id
{-# INLINE transformH #-}

translate :: forall e e' ef eh. (e' <! ef) => (e ~> e') -> Eff eh (e ': ef) ~> Eff eh ef
translate f = transEffHF id (either id (inj . f) . decomp)
{-# INLINE translate #-}

translateH
    :: forall e e' eh ef
     . (e' <!! eh, HFunctor e)
    => (e (Eff eh ef) ~> e' (Eff eh ef))
    -> Eff (e ': eh) ef ~> Eff eh ef
translateH f = transEffHF (either id (injH . f) . decompH) id
{-# INLINE translateH #-}

rewrite :: forall e ef eh. (e <! ef) => (e ~> e) -> Eff eh ef ~> Eff eh ef
rewrite f = transEffHF id \u -> maybe u (inj . f) $ prj @e u
{-# INLINE rewrite #-}

rewriteH
    :: forall e eh ef
     . (e <!! eh, HFunctor e)
    => (e (Eff eh ef) ~> e (Eff eh ef))
    -> Eff eh ef ~> Eff eh ef
rewriteH f = transEffHF (\u -> maybe u (injH . f) $ prjH @e u) id
{-# INLINE rewriteH #-}

raise :: (ef `IsSuffixOf` ef') => Eff eh ef ~> Eff eh ef'
raise = transEffHF id weakens
{-# INLINE raise #-}

raiseN :: forall len ef ef' eh. (Weaken len ef ef') => Eff eh ef ~> Eff eh ef'
raiseN = transEffHF id (weakenN @len)
{-# INLINE raiseN #-}

raiseNUnder
    :: forall len offset ef ef' eh
     . (WeakenUnder len offset ef ef')
    => Eff eh ef ~> Eff eh ef'
raiseNUnder = transEffHF id (weakenNUnderM @len @offset)
{-# INLINE raiseNUnder #-}

raiseH :: (eh `IsSuffixOf` eh') => Eff eh ef ~> Eff eh' ef
raiseH = transEffHF weakensH id
{-# INLINE raiseH #-}

raiseNH :: forall len eh eh' ef. (Weaken len eh eh') => Eff eh ef ~> Eff eh' ef
raiseNH = transEffHF (weakenNH @len) id
{-# INLINE raiseNH #-}

raiseNUnderH
    :: forall len offset eh eh' ef
     . (WeakenUnder len offset eh eh')
    => Eff eh ef ~> Eff eh' ef
raiseNUnderH = transEffHF (weakenNUnderMH @len @offset) id
{-# INLINE raiseNUnderH #-}

subsumeN :: forall len ef ef' eh. (Strengthen len ef ef') => Eff eh ef ~> Eff eh ef'
subsumeN = transEffHF id (strengthenN @len)
{-# INLINE subsumeN #-}

subsumeNUnder
    :: forall len offset ef ef' eh
     . (StrengthenUnder len offset ef ef')
    => Eff eh ef ~> Eff eh ef'
subsumeNUnder = transEffHF id (strengthenNUnderM @len @offset)
{-# INLINE subsumeNUnder #-}

subsumeNH :: forall len eh eh' ef. (Strengthen len eh eh') => Eff eh ef ~> Eff eh' ef
subsumeNH = transEffHF (strengthenNH @len) id
{-# INLINE subsumeNH #-}

subsumeNUnderH
    :: forall len offset eh eh' ef
     . (StrengthenUnder len offset eh eh')
    => Eff eh ef ~> Eff eh' ef
subsumeNUnderH = transEffHF (strengthenNUnderMH @len @offset) id
{-# INLINE subsumeNUnderH #-}

-- todo: add raiseUnder(H), subsume(H), subsumeUnder(H)

bundle
    :: forall ef bundle rest eh
     . (Bundle ef bundle rest)
    => Eff eh ef ~> Eff eh (Union bundle ': rest)
bundle = transEffHF id bundleUnion
{-# INLINE bundle #-}

unbundle
    :: forall ef bundle rest eh
     . (Bundle ef bundle rest)
    => Eff eh (Union bundle ': rest) ~> Eff eh ef
unbundle = transEffHF id unbundleUnion
{-# INLINE unbundle #-}

bundleUnder
    :: forall offset bundle ef ef' eh
     . (BundleUnder Union offset ef ef' bundle)
    => Eff eh ef ~> Eff eh ef'
bundleUnder = transEffHF id (bundleUnionUnder @offset)
{-# INLINE bundleUnder #-}

unbundleUnder
    :: forall offset bundle ef ef' eh
     . (BundleUnder Union offset ef ef' bundle)
    => Eff eh ef' ~> Eff eh ef
unbundleUnder = transEffHF id (unbundleUnionUnder @offset)
{-# INLINE unbundleUnder #-}

bundleAll :: Eff eh ef ~> Eff eh '[Union ef]
bundleAll = transEffHF id bundleAllUnion
{-# INLINE bundleAll #-}

unbundleAll :: Eff eh '[Union ef] ~> Eff eh ef
unbundleAll = transEffHF id unbundleAllUnion
{-# INLINE unbundleAll #-}

bundleH
    :: forall eh bundle rest ef
     . (Bundle eh bundle rest)
    => Eff eh ef ~> Eff (UnionH bundle ': rest) ef
bundleH = transEffHF bundleUnionH id
{-# INLINE bundleH #-}

unbundleH
    :: forall eh bundle rest ef
     . (Bundle eh bundle rest)
    => Eff (UnionH bundle ': rest) ef ~> Eff eh ef
unbundleH = transEffHF unbundleUnionH id
{-# INLINE unbundleH #-}

bundleUnderH
    :: forall offset bundle eh eh' ef
     . (BundleUnder UnionH offset eh eh' bundle)
    => Eff eh ef ~> Eff eh' ef
bundleUnderH = transEffHF (bundleUnionUnderH @offset) id
{-# INLINE bundleUnderH #-}

unbundleUnderH
    :: forall offset bundle eh eh' ef
     . (BundleUnder UnionH offset eh eh' bundle)
    => Eff eh' ef ~> Eff eh ef
unbundleUnderH = transEffHF (unbundleUnionUnderH @offset) id
{-# INLINE unbundleUnderH #-}

bundleAllH :: Eff eh ef ~> Eff '[UnionH eh] ef
bundleAllH = transEffHF bundleAllUnionH id
{-# INLINE bundleAllH #-}

unbundleAllH :: Eff '[UnionH eh] ef ~> Eff eh ef
unbundleAllH = transEffHF unbundleAllUnionH id
{-# INLINE unbundleAllH #-}

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

a = runPure $ interpret undefined $ interpret @(Union '[State Int, State Char]) undefined $ bundle $ pure @(Eff '[] '[State Int, State Char, State ()]) ()
