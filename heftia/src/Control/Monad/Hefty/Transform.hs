{-# LANGUAGE AllowAmbiguousTypes #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file)
Maintainer  :  ymdfield@outlook.jp

This module provides functions for transforming effects.
Please refer to the documentation of the [top-level module]("Control.Monad.Hefty").
-}
module Control.Monad.Hefty.Transform where

import Control.Effect (type (~>))
import Control.Monad.Hefty.Interpret (iterAllEffHFBy)
import Control.Monad.Hefty.Types (Eff, sendUnionBy, sendUnionHBy)
import Data.Effect.HFunctor (HFunctor)
import Data.Effect.Key (Key (Key, unKey), KeyH (KeyH, unKeyH), type (##>), type (#>))
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
    WeakenUnder,
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
    strengthen,
    strengthenN,
    strengthenNUnder,
    strengthenUnder,
    strengthens,
    strengthensUnder,
    unbundleAllUnion,
    unbundleUnion,
    unbundleUnionN,
    unbundleUnionUnder,
    weaken,
    weakenN,
    weakenNUnder,
    weakenUnder,
    weakens,
    weakensUnder,
    type (<|),
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
    strengthenH,
    strengthenNH,
    strengthenNUnderH,
    strengthenUnderH,
    strengthensH,
    unbundleAllUnionH,
    unbundleUnionH,
    unbundleUnionUnderH,
    weakenH,
    weakenNH,
    weakenNUnderH,
    weakenUnderH,
    weakensH,
    type (<<|),
 )
import Data.Effect.Tag (Tag (Tag, unTag), TagH (TagH, unTagH), type (#), type (##))
import GHC.TypeNats (KnownNat)

-- * Rewriting effectful operations

{- | Transforms the first-order effect @e@ at the head of the list into another
first-order effect @e'@.
-}
transform
    :: forall e e' ef eh
     . (e ~> e')
    -> Eff eh (e ': ef) ~> Eff eh (e' ': ef)
transform f = transEff (either weaken (inj . f) . decomp)
{-# INLINE transform #-}

{- | Transforms the higher-order effect @e@ at the head of the list into another
higher-order effect @e'@.
-}
transformH
    :: forall e e' eh ef
     . (HFunctor e)
    => (e (Eff (e' ': eh) ef) ~> e' (Eff (e' ': eh) ef))
    -> Eff (e ': eh) ef ~> Eff (e' ': eh) ef
transformH f = transEffH (either weakenH (injH . f) . decompH)
{-# INLINE transformH #-}

{- | Transforms the first-order effect @e@ at the head of the list into another
first-order effect @e'@ and embeds it into the list.

If multiple instances of @e'@ exist in the list, the one closest to the head
(with the smallest index) will be targeted.
-}
translate
    :: forall e e' ef eh
     . (e' <| ef)
    => (e ~> e')
    -> Eff eh (e ': ef) ~> Eff eh ef
translate f = transEff (either id (inj . f) . decomp)
{-# INLINE translate #-}

{- | Transforms the higher-order effect @e@ at the head of the list into another
higher-order effect @e'@ and embeds it into the list.

If multiple instances of @e'@ exist in the list, the one closest to the head
(with the smallest index) will be targeted.
-}
translateH
    :: forall e e' eh ef
     . (e' <<| eh, HFunctor e)
    => (e (Eff eh ef) ~> e' (Eff eh ef))
    -> Eff (e ': eh) ef ~> Eff eh ef
translateH f = transEffH (either id (injH . f) . decompH)
{-# INLINE translateH #-}

{- | Rewrites the first-order effect @e@ in the list.

If multiple instances of @e@ exist in the list, the one closest to the head
(with the smallest index) will be targeted.
-}
rewrite
    :: forall e ef eh
     . (e <| ef)
    => (e ~> e)
    -> Eff eh ef ~> Eff eh ef
rewrite f = transEff \u -> maybe u (inj . f) $ prj @e u
{-# INLINE rewrite #-}

{- | Rewrites the higher-order effect @e@ in the list.

If multiple instances of @e@ exist in the list, the one closest to the head
(with the smallest index) will be targeted.
-}
rewriteH
    :: forall e eh ef
     . (e <<| eh, HFunctor e)
    => (e (Eff eh ef) ~> e (Eff eh ef))
    -> Eff eh ef ~> Eff eh ef
rewriteH f = transEffH \u -> maybe u (injH . f) $ prjH @e u
{-# INLINE rewriteH #-}

-- | Transforms all first-order effects in the open union at once.
transEff
    :: forall ef ef' eh
     . (Union ef ~> Union ef')
    -> Eff eh ef ~> Eff eh ef'
transEff = transEffHF id
{-# INLINE transEff #-}

-- | Transforms all higher-order effects in the open union at once.
transEffH
    :: forall eh eh' ef
     . (UnionH eh (Eff eh' ef) ~> UnionH eh' (Eff eh' ef))
    -> Eff eh ef ~> Eff eh' ef
transEffH f = transEffHF f id
{-# INLINE transEffH #-}

{- | Transforms all higher-order and first-order effects in the open union at
once.
-}
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

-- * Manipulating the effect list (without rewriting effectful operations)

-- ** Insertion functions

-- | Adds an arbitrary first-order effect @e@ to the head of the list.
raise :: forall e ef eh. Eff eh ef ~> Eff eh (e ': ef)
raise = transEff weaken
{-# INLINE raise #-}

-- | Adds multiple arbitrary first-order effects to the head of the list.
raises :: (ef `IsSuffixOf` ef') => Eff eh ef ~> Eff eh ef'
raises = transEff weakens
{-# INLINE raises #-}

{- | Adds a specified number @len@ of arbitrary first-order effects to the head
of the list.
-}
raiseN
    :: forall len ef ef' eh
     . (WeakenN len ef ef')
    => Eff eh ef ~> Eff eh ef'
raiseN = transEff (weakenN @len)
{-# INLINE raiseN #-}

{- | Inserts an arbitrary first-order effect @e2@ just below the head of the
list.
-}
raiseUnder
    :: forall e1 e2 ef eh
     . Eff eh (e1 ': ef) ~> Eff eh (e1 ': e2 ': ef)
raiseUnder = transEff weakenUnder
{-# INLINE raiseUnder #-}

{- | Inserts multiple arbitrary first-order effects at a position @offset@ steps
below the head of the list.
-}
raisesUnder
    :: forall offset ef ef' eh
     . (WeakenUnder offset ef ef')
    => Eff eh ef ~> Eff eh ef'
raisesUnder = transEff (weakensUnder @offset)
{-# INLINE raisesUnder #-}

{- | Inserts @len@ arbitrary first-order effects at a position @offset@ steps
below the head of the list.
-}
raiseNUnder
    :: forall len offset ef ef' eh
     . (WeakenNUnder len offset ef ef')
    => Eff eh ef ~> Eff eh ef'
raiseNUnder = transEff (weakenNUnder @len @offset)
{-# INLINE raiseNUnder #-}

{- | Adds a specified number @len@ of arbitrary higher-order effects to the head
of the list.
-}
raiseH :: forall e eh ef. Eff eh ef ~> Eff (e ': eh) ef
raiseH = transEffH weakenH
{-# INLINE raiseH #-}

{- | Inserts an arbitrary higher-order effect @e2@ just below the head of the
list.
-}
raisesH :: (eh `IsSuffixOf` eh') => Eff eh ef ~> Eff eh' ef
raisesH = transEffH weakensH
{-# INLINE raisesH #-}

{- | Adds a specified number @len@ of arbitrary higher-order effects to the head
of the list.
-}
raiseNH
    :: forall len eh eh' ef
     . (WeakenN len eh eh')
    => Eff eh ef ~> Eff eh' ef
raiseNH = transEffH (weakenNH @len)
{-# INLINE raiseNH #-}

{- | Inserts an arbitrary higher-order effect @e2@ just below the head of the
list.
-}
raiseUnderH
    :: forall e1 e2 eh ef
     . Eff (e1 ': eh) ef ~> Eff (e1 ': e2 ': eh) ef
raiseUnderH = transEffH weakenUnderH
{-# INLINE raiseUnderH #-}

{- | Inserts @len@ arbitrary higher-order effects at a position @offset@ steps
below the head of the list.
-}
raiseNUnderH
    :: forall len offset eh eh' ef
     . (WeakenNUnder len offset eh eh')
    => Eff eh ef ~> Eff eh' ef
raiseNUnderH = transEffH (weakenNUnderH @len @offset)
{-# INLINE raiseNUnderH #-}

-- ** Merging functions

{- | Merges the first first-order effect @e@ at the head of the list into the
same type of effect @e@ that is below it.

If multiple candidates of @e@ exist in the list, the one closest to the head
(with the smallest index) will be targeted.
-}
subsume
    :: forall e ef eh
     . (e <| ef)
    => Eff eh (e ': ef) ~> Eff eh ef
subsume = transEff strengthen
{-# INLINE subsume #-}

{- | Merges multiple first-order effects at the head of the list into effects of
the same types that are below them.
-}
subsumes
    :: forall ef ef' eh
     . (Strengthen ef ef')
    => Eff eh ef ~> Eff eh ef'
subsumes = transEff strengthens
{-# INLINE subsumes #-}

{- | Merges a specified number @len@ of first-order effects at the head of the
list into effects of the same types that are below them.
-}
subsumeN
    :: forall len ef ef' eh
     . (StrengthenN len ef ef')
    => Eff eh ef ~> Eff eh ef'
subsumeN = transEff (strengthenN @len)
{-# INLINE subsumeN #-}

{- | Merges the first-order effect @e2@ located just below the head into the
same type of effect @e2@ that is below it.
-}
subsumeUnder
    :: forall e2 e1 ef eh
     . (e2 <| ef)
    => Eff eh (e1 ': e2 ': ef) ~> Eff eh (e1 ': ef)
subsumeUnder = transEff strengthenUnder
{-# INLINE subsumeUnder #-}

{- | Merges multiple first-order effects at an @offset@ below the head into
effects of the same types that are below them.
-}
subsumesUnder
    :: forall offset ef ef' eh
     . (StrengthenUnder offset ef ef')
    => Eff eh ef ~> Eff eh ef'
subsumesUnder = transEff (strengthensUnder @offset)
{-# INLINE subsumesUnder #-}

{- | Merges @len@ first-order effects at an @offset@ below the head into effects
of the same types that are below them.
-}
subsumeNUnder
    :: forall len offset ef ef' eh
     . (StrengthenNUnder len offset ef ef')
    => Eff eh ef ~> Eff eh ef'
subsumeNUnder = transEff (strengthenNUnder @len @offset)
{-# INLINE subsumeNUnder #-}

{- | Merges the first higher-order effect @e@ at the head of the list into the
same type of effect @e@ that is below it.

If multiple candidates of @e@ exist in the list, the one closest to the head
(with the smallest index) will be targeted.
-}
subsumeH
    :: forall e eh ef
     . (e <<| eh)
    => Eff (e ': eh) ef ~> Eff eh ef
subsumeH = transEffH strengthenH
{-# INLINE subsumeH #-}

{- | Merges multiple higher-order effects at the head of the list into effects of
the same types that are below them.
-}
subsumesH
    :: forall eh eh' ef
     . (Strengthen eh eh')
    => Eff eh ef ~> Eff eh' ef
subsumesH = transEffH strengthensH
{-# INLINE subsumesH #-}

{- | Merges a specified number @len@ of higher-order effects at the head of the
list into effects of the same types that are below them.
-}
subsumeNH
    :: forall len eh eh' ef
     . (StrengthenN len eh eh')
    => Eff eh ef ~> Eff eh' ef
subsumeNH = transEffH (strengthenNH @len)
{-# INLINE subsumeNH #-}

{- | Merges the higher-order effect @e2@ located just below the head into the
same type of effect @e2@ that is below it.
-}
subsumeUnderH
    :: forall e2 e1 eh ef
     . (e2 <<| eh)
    => Eff (e1 ': e2 ': eh) ef ~> Eff (e1 ': eh) ef
subsumeUnderH = transEffH strengthenUnderH
{-# INLINE subsumeUnderH #-}

{- | Merges @len@ higher-order effects at an @offset@ below the head into effects
of the same types that are below them.
-}
subsumeNUnderH
    :: forall len offset eh eh' ef
     . (StrengthenNUnder len offset eh eh')
    => Eff eh ef ~> Eff eh' ef
subsumeNUnderH = transEffH (strengthenNUnderH @len @offset)
{-# INLINE subsumeNUnderH #-}

-- ** Bundling functions

{- | Bundles several effects at the head of the list into a single element using
an open union.
-}
bundle
    :: forall ef bundle rest eh
     . (Split ef bundle rest)
    => Eff eh ef ~> Eff eh (Union bundle ': rest)
bundle = transEff bundleUnion
{-# INLINE bundle #-}

{- | Bundles the first @len@ effects at the head of the list into a single
element using an open union.
-}
bundleN
    :: forall len ef eh
     . (KnownNat len)
    => Eff eh ef ~> Eff eh (Union (Take len ef) ': Drop len ef)
bundleN = transEff (bundleUnionN @len)
{-# INLINE bundleN #-}

-- | Expands effects that have been bundled into an open union.
unbundle
    :: forall ef bundle rest eh
     . (Split ef bundle rest)
    => Eff eh (Union bundle ': rest) ~> Eff eh ef
unbundle = transEff unbundleUnion
{-# INLINE unbundle #-}

-- | Expands the first @len@ effects that have been bundled into an open union.
unbundleN
    :: forall len ef eh
     . (KnownNat len)
    => Eff eh (Union (Take len ef) ': Drop len ef) ~> Eff eh ef
unbundleN = transEff (unbundleUnionN @len)
{-# INLINE unbundleN #-}

{- | Expands effects at an @offset@ below the head of the list into a single
element using an open union.
-}
bundleUnder
    :: forall offset bundle ef ef' eh
     . (BundleUnder Union offset ef ef' bundle)
    => Eff eh ef ~> Eff eh ef'
bundleUnder = transEff (bundleUnionUnder @offset)
{-# INLINE bundleUnder #-}

-- TODO: add *bundle*N(H) functions

{- | Expands effects that have been bundled into an open union at an @offset@
below the head of the list.
-}
unbundleUnder
    :: forall offset bundle ef ef' eh
     . (BundleUnder Union offset ef ef' bundle)
    => Eff eh ef' ~> Eff eh ef
unbundleUnder = transEff (unbundleUnionUnder @offset)
{-# INLINE unbundleUnder #-}

-- | Bundles all first-order effects into a single open union.
bundleAll :: Eff eh ef ~> Eff eh '[Union ef]
bundleAll = transEff bundleAllUnion
{-# INLINE bundleAll #-}

-- | Expands all first-order effects from a single open union.
unbundleAll :: Eff eh '[Union ef] ~> Eff eh ef
unbundleAll = transEff unbundleAllUnion
{-# INLINE unbundleAll #-}

{- | Bundles several effects at the head of the list into a single element using
an open union.
-}
bundleH
    :: forall eh bundle rest ef
     . (Split eh bundle rest)
    => Eff eh ef ~> Eff (UnionH bundle ': rest) ef
bundleH = transEffH bundleUnionH
{-# INLINE bundleH #-}

-- | Expands effects that have been bundled into an open union.
unbundleH
    :: forall eh bundle rest ef
     . (Split eh bundle rest)
    => Eff (UnionH bundle ': rest) ef ~> Eff eh ef
unbundleH = transEffH unbundleUnionH
{-# INLINE unbundleH #-}

{- | Expands effects at an @offset@ below the head of the list into a single
element using an open union.
-}
bundleUnderH
    :: forall offset bundle eh eh' ef
     . (BundleUnder UnionH offset eh eh' bundle)
    => Eff eh ef ~> Eff eh' ef
bundleUnderH = transEffH (bundleUnionUnderH @offset)
{-# INLINE bundleUnderH #-}

{- | Expands effects that have been bundled into an open union at an @offset@
below the head of the list.
-}
unbundleUnderH
    :: forall offset bundle eh eh' ef
     . (BundleUnder UnionH offset eh eh' bundle)
    => Eff eh' ef ~> Eff eh ef
unbundleUnderH = transEffH (unbundleUnionUnderH @offset)
{-# INLINE unbundleUnderH #-}

-- | Bundles all higher-order effects into a single open union.
bundleAllH :: Eff eh ef ~> Eff '[UnionH eh] ef
bundleAllH = transEffH bundleAllUnionH
{-# INLINE bundleAllH #-}

-- | Expands all higher-order effects from a single open union.
unbundleAllH :: Eff '[UnionH eh] ef ~> Eff eh ef
unbundleAllH = transEffH unbundleAllUnionH
{-# INLINE unbundleAllH #-}

-- ** Manipulating Tags & Keys

-- | Attaches the @tag@ to the first-order effect at the head of the list.
tag
    :: forall tag e ef eh
     . Eff eh (e ': ef) ~> Eff eh (e # tag ': ef)
tag = transform Tag
{-# INLINE tag #-}

-- | Removes the @tag@ from the tagged first-order effect at the head of the list.
untag
    :: forall tag e ef eh
     . Eff eh (e # tag ': ef) ~> Eff eh (e ': ef)
untag = transform unTag
{-# INLINE untag #-}

{- | Changes the @tag@ of the tagged first-order effect at the head of the list
to another tag @tag'@.
-}
retag
    :: forall tag' tag e ef eh
     . Eff eh (e # tag ': ef) ~> Eff eh (e # tag' ': ef)
retag = transform $ Tag . unTag
{-# INLINE retag #-}

-- | Attaches the @tag@ to the higher-order effect at the head of the list.
tagH
    :: forall tag e ef eh
     . (HFunctor e)
    => Eff (e ': eh) ef ~> Eff (e ## tag ': eh) ef
tagH = transformH TagH
{-# INLINE tagH #-}

{- | Removes the @tag@ from the tagged higher-order effect at the head of the
 list.
-}
untagH
    :: forall tag e eh ef
     . (HFunctor e)
    => Eff (e ## tag ': eh) ef ~> Eff (e ': eh) ef
untagH = transformH unTagH
{-# INLINE untagH #-}

{- | Changes the @tag@ of the tagged higher-order effect at the head of the list
to another tag @tag'@.
-}
retagH
    :: forall tag' tag e eh ef
     . (HFunctor e)
    => Eff (e ## tag ': eh) ef ~> Eff (e ## tag' ': eh) ef
retagH = transformH $ TagH . unTagH
{-# INLINE retagH #-}

-- | Removes the @key@ from the keyed first-order effect at the head of the list.
unkey
    :: forall key e ef eh
     . Eff eh (key #> e ': ef) ~> Eff eh (e ': ef)
unkey = transform unKey
{-# INLINE unkey #-}

{- | Changes the @key@ of the keyed first-order effect at the head of the list
to another key @key'@.
-}
rekey
    :: forall key' key e ef eh
     . Eff eh (key #> e ': ef) ~> Eff eh (key' #> e ': ef)
rekey = transform $ Key . unKey
{-# INLINE rekey #-}

-- | Removes the @key@ from the keyed higher-order effect at the head of the list.
unkeyH
    :: forall key e eh ef
     . (HFunctor e)
    => Eff (key ##> e ': eh) ef ~> Eff (e ': eh) ef
unkeyH = transformH unKeyH
{-# INLINE unkeyH #-}

{- | Changes the @key@ of the keyed higher-order effect at the head of the list
to another key @key'@.
-}
rekeyH
    :: forall key' key e eh ef
     . (HFunctor e)
    => Eff (key ##> e ': eh) ef ~> Eff (key' ##> e ': eh) ef
rekeyH = transformH $ KeyH . unKeyH
{-# INLINE rekeyH #-}
