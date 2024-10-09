{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

-- SPDX-License-Identifier: MPL-2.0 AND BSD-3-Clause

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

{-
Copyright (c) 2016, Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Allele Dev nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{- |
Copyright   :  (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King; 2024 Sayo Koyoneda
License     :  MPL-2.0 (see the LICENSE file) AND BSD-3-Clause
Maintainer  :  ymdfield@outlook.jp
Description :  Open unions (type-indexed co-products) for extensible higher-order effects.

Implementation of an open union for higher-order effects.

Importing this module allows unsafe access to the data structure of the open
union, so it should not usually be imported directly.

Based on [the open union in freer-simple](https://hackage.haskell.org/package/freer-simple-1.2.1.2/docs/Data-OpenUnion-Internal.html).
-}
module Data.Effect.OpenUnion.Internal.HO where

import Control.Effect (type (~>))
import Data.Coerce (coerce)
import Data.Effect (EffectH, IsHFunctor)
import Data.Effect.HFunctor (HFunctor, hfmap)
import Data.Effect.Key (type (##>))
import Data.Effect.OpenUnion.Internal (
    BundleUnder,
    Drop,
    ElemAt,
    ElemIndex,
    FindElem,
    IfNotFound,
    IsSuffixOf,
    KnownLength,
    Length,
    LookupError,
    P (unP),
    PrefixLength,
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
    elemNo,
    prefixLen,
    reifyLength,
    strengthenMap,
    strengthenMapUnder,
    wordVal,
    type (++),
 )
import Data.Kind (Type)
import Data.Type.Bool (type (&&))
import GHC.TypeNats (KnownNat, type (-))
import Unsafe.Coerce (unsafeCoerce)

-- | Open union for higher-order effects.
data UnionH (es :: [EffectH]) (f :: Type -> Type) (a :: Type) where
    UnionH
        :: {-# UNPACK #-} !Word
        -- ^ A natural number tag to identify the element of the union.
        -> e g a
        -- ^ The data of the higher-order effect that is an element of the union.
        -> (g ~> f)
        -- ^ Continuation of interpretation.
        -- Due to this component, v'hfmap' for t'UnionH' becomes faster (because
        -- it no longer requires the t'HFunctor' dictionary), thus improving overall performance.
        -> UnionH es f a

class (IsHFunctors es ~ 'True) => HFunctors es
instance HFunctors '[]
instance (IsHFunctor e ~ 'True, HFunctors es) => HFunctors (e ': es)

type NotHFunctor e = IsHFunctor e ~ 'False

type family IsHFunctors es where
    IsHFunctors '[] = 'True
    IsHFunctors (e ': es) = IsHFunctor e && IsHFunctors es

hfmapUnion :: (HFunctors es) => (f ~> g) -> UnionH es f a -> UnionH es g a
hfmapUnion phi (UnionH n e koi) = UnionH n e (phi . koi)
{-# INLINE hfmapUnion #-}

instance (HFunctors es) => HFunctor (UnionH es) where
    hfmap f = hfmapUnion f
    {-# INLINE hfmap #-}

type instance IsHFunctor (UnionH es) = IsHFunctors es

unsafeInjH :: Word -> e f a -> UnionH es f a
unsafeInjH n e = UnionH n e id
{-# INLINE unsafeInjH #-}

unsafePrjH :: (HFunctor e) => Word -> UnionH es f a -> Maybe (e f a)
unsafePrjH n (UnionH n' e koi)
    | n == n' = Just (hfmap koi $ unsafeCoerce e)
    | otherwise = Nothing
{-# INLINE unsafePrjH #-}

unsafePrjH_ :: (NotHFunctor e) => Word -> UnionH es f a -> Maybe (e f a)
unsafePrjH_ n (UnionH n' e _)
    | n == n' = Just (unsafeCoerce e)
    | otherwise = Nothing
{-# INLINE unsafePrjH_ #-}

class (FindElem e es) => MemberH (e :: EffectH) es where
    injH :: e f a -> UnionH es f a
    prjH :: (HFunctor e) => UnionH es f a -> Maybe (e f a)
    prjH_ :: (NotHFunctor e) => UnionH es f a -> Maybe (e f a)

instance (FindElem e es, IfNotFound e es es) => MemberH e es where
    injH = unsafeInjH $ unP (elemNo :: P e es)
    {-# INLINE injH #-}

    prjH = unsafePrjH $ unP (elemNo :: P e es)
    {-# INLINE prjH #-}

    prjH_ = unsafePrjH_ $ unP (elemNo :: P e es)
    {-# INLINE prjH_ #-}

infix 3 <<|
type (<<|) = MemberH

type MemberHBy key e es = (key ##> e <<| es, LookupH key es ~ key ##> e)

type LookupH key es = LookupH_ key es es

type family LookupH_ (key :: k) r w :: EffectH where
    LookupH_ key (key ##> e ': _) w = key ##> e
    LookupH_ key (_ ': r) w = LookupH_ key r w
    LookupH_ key '[] w = LookupError key w

decompH :: (HFunctor e) => UnionH (e ': es) f a -> Either (UnionH es f a) (e f a)
decompH (UnionH 0 a koi) = Right $ hfmap koi $ unsafeCoerce a
decompH (UnionH n a koi) = Left $ UnionH (n - 1) a koi
{-# INLINE [2] decompH #-}

decomp0H :: (HFunctor e) => UnionH '[e] f a -> Either (UnionH '[] f a) (e f a)
decomp0H (UnionH _ a koi) = Right $ hfmap koi $ unsafeCoerce a
{-# INLINE decomp0H #-}
{-# RULES "decomp/singleton" decompH = decomp0H #-}

infixr 5 !!+
(!!+) :: (HFunctor e) => (e f a -> r) -> (UnionH es f a -> r) -> UnionH (e : es) f a -> r
(f !!+ g) u = case decompH u of
    Left x -> g x
    Right x -> f x
{-# INLINE (!!+) #-}

decompH_ :: (NotHFunctor e) => UnionH (e ': es) f a -> Either (UnionH es f a) (e f a)
decompH_ (UnionH 0 a _) = Right $ unsafeCoerce a
decompH_ (UnionH n a koi) = Left $ UnionH (n - 1) a koi
{-# INLINE [2] decompH_ #-}

decomp0H_ :: (NotHFunctor e) => UnionH '[e] f a -> Either (UnionH '[] f a) (e f a)
decomp0H_ (UnionH _ a _) = Right $ unsafeCoerce a
{-# INLINE decomp0H_ #-}
{-# RULES "decomp/singleton" decompH_ = decomp0H_ #-}

infixr 5 !!+.
(!!+.) :: (NotHFunctor e) => (e f a -> r) -> (UnionH es f a -> r) -> UnionH (e : es) f a -> r
(f !!+. g) u = case decompH_ u of
    Left x -> g x
    Right x -> f x
{-# INLINE (!!+.) #-}

infixr 5 $+
($+) :: (NotHFunctor e) => (p -> e f a -> r) -> (p -> UnionH es f a -> r) -> p -> UnionH (e : es) f a -> r
(f $+ g) b u = case decompH_ u of
    Left x -> g b x
    Right x -> f b x
{-# INLINE ($+) #-}

extractH :: (HFunctor e) => UnionH '[e] f a -> e f a
extractH (UnionH _ a koi) = hfmap koi $ unsafeCoerce a
{-# INLINE extractH #-}

extractH_ :: (NotHFunctor e) => UnionH '[e] f a -> e f a
extractH_ (UnionH _ a _) = unsafeCoerce a
{-# INLINE extractH_ #-}

inj0H :: forall e es f a. e f a -> UnionH (e ': es) f a
inj0H a = UnionH 0 a id
{-# INLINE inj0H #-}

injNH :: forall i es f a. (KnownNat i) => ElemAt i es f a -> UnionH es f a
injNH a = UnionH (wordVal @i) a id
{-# INLINE injNH #-}

prjNH
    :: forall i es f a
     . (KnownNat i, HFunctor (ElemAt i es))
    => UnionH es f a
    -> Maybe (ElemAt i es f a)
prjNH (UnionH n a koi)
    | n == wordVal @i = Just $ hfmap koi $ unsafeCoerce a
    | otherwise = Nothing
{-# INLINE prjNH #-}

prjNH_
    :: forall i es f a
     . (KnownNat i, NotHFunctor (ElemAt i es))
    => UnionH es f a
    -> Maybe (ElemAt i es f a)
prjNH_ (UnionH n a _)
    | n == wordVal @i = Just $ unsafeCoerce a
    | otherwise = Nothing
{-# INLINE prjNH_ #-}

weakenH :: forall any es f a. UnionH es f a -> UnionH (any ': es) f a
weakenH (UnionH n a koi) = UnionH (n + 1) a koi
{-# INLINE weakenH #-}

weakensH :: forall es es' f a. (es `IsSuffixOf` es') => UnionH es f a -> UnionH es' f a
weakensH (UnionH n a koi) = UnionH (n + prefixLen @es @es') a koi
{-# INLINE weakensH #-}

weakenNH :: forall len es es' f a. (WeakenN len es es') => UnionH es f a -> UnionH es' f a
weakenNH (UnionH n a koi) = UnionH (n + wordVal @len) a koi
{-# INLINE weakenNH #-}

weakenUnderH :: forall any e es f a. UnionH (e ': es) f a -> UnionH (e ': any ': es) f a
weakenUnderH u@(UnionH n a koi)
    | n == 0 = coerce u
    | otherwise = UnionH (n + 1) a koi
{-# INLINE weakenUnderH #-}

weakensUnderH :: forall offset es es' f a. (WeakenUnder offset es es') => UnionH es f a -> UnionH es' f a
weakensUnderH = weakenNUnderH @(PrefixLength es es') @offset
{-# INLINE weakensUnderH #-}

weakenNUnderH
    :: forall len offset es es' f a
     . (WeakenNUnder len offset es es')
    => UnionH es f a
    -> UnionH es' f a
weakenNUnderH u@(UnionH n a koi)
    | n < wordVal @offset = coerce u
    | otherwise = UnionH (n + wordVal @len) a koi
{-# INLINE weakenNUnderH #-}

strengthenH :: forall e es f a. (e <<| es) => UnionH (e ': es) f a -> UnionH es f a
strengthenH (UnionH n a koi)
    | n == 0 = UnionH (wordVal @(ElemIndex e es)) a koi
    | otherwise = UnionH (n - 1) a koi
{-# INLINE strengthenH #-}

strengthensH :: forall es es' f a. (Strengthen es es') => UnionH es f a -> UnionH es' f a
strengthensH = strengthenNH @(PrefixLength es' es)
{-# INLINE strengthensH #-}

strengthenNH :: forall len es es' f a. (StrengthenN len es es') => UnionH es f a -> UnionH es' f a
strengthenNH (UnionH n a koi) = UnionH (strengthenMap @_ @_ @len @es @es' n) a koi
{-# INLINE strengthenNH #-}

strengthenUnderH :: forall e2 e1 es f a. (e2 <<| es) => UnionH (e1 ': e2 ': es) f a -> UnionH (e1 ': es) f a
strengthenUnderH u@(UnionH n a koi)
    | n == 0 = coerce u
    | n == 1 = UnionH (1 + wordVal @(ElemIndex e2 es)) a koi
    | otherwise = UnionH (n - 1) a koi
{-# INLINE strengthenUnderH #-}

strengthensUnderH
    :: forall offset es es' f a
     . (StrengthenUnder offset es es')
    => UnionH es f a
    -> UnionH es' f a
strengthensUnderH = strengthenNUnderH @(PrefixLength es' es) @offset
{-# INLINE strengthensUnderH #-}

strengthenNUnderH
    :: forall len offset es es' f a
     . (StrengthenNUnder len offset es es')
    => UnionH es f a
    -> UnionH es' f a
strengthenNUnderH u@(UnionH n a koi)
    | n < off = coerce u
    | otherwise = UnionH (off + strengthenMapUnder @len @offset @es @es' (n - off)) a koi
  where
    off = wordVal @offset
{-# INLINE strengthenNUnderH #-}

bundleUnionH
    :: forall bundle es rest f a
     . (Split es bundle rest)
    => UnionH es f a
    -> UnionH (UnionH bundle ': rest) f a
bundleUnionH = bundleUnionNH @(Length bundle)
{-# INLINE bundleUnionH #-}

bundleUnionNH
    :: forall len es f a
     . (KnownNat len)
    => UnionH es f a
    -> UnionH (UnionH (Take len es) ': Drop len es) f a
bundleUnionNH (UnionH n a koi)
    | n < len = UnionH 0 (UnionH n a koi) id
    | otherwise = UnionH (n - len + 1) a koi
  where
    len = wordVal @len
{-# INLINE bundleUnionNH #-}

unbundleUnionH
    :: forall bundle es rest f a
     . (Split es bundle rest)
    => UnionH (UnionH bundle ': rest) f a
    -> UnionH es f a
unbundleUnionH = unbundleUnionNH @(Length bundle)
{-# INLINE unbundleUnionH #-}

unbundleUnionNH
    :: forall len es f a
     . (KnownNat len)
    => UnionH (UnionH (Take len es) ': Drop len es) f a
    -> UnionH es f a
unbundleUnionNH (UnionH n a koi)
    | n == 0 = case unsafeCoerce a of
        UnionH n' a' koi' -> UnionH n' a' (koi . koi')
    | otherwise = UnionH (n - 1 + wordVal @len) a koi
{-# INLINE unbundleUnionNH #-}

bundleUnionUnderH
    :: forall offset bundle es es' f a
     . (BundleUnder UnionH offset es es' bundle)
    => UnionH es f a
    -> UnionH es' f a
bundleUnionUnderH = bundleUnionNUnderH @(Length bundle) @offset
{-# INLINE bundleUnionUnderH #-}

bundleUnionNUnderH
    :: forall len offset es f a
     . (KnownNat len, KnownNat offset)
    => UnionH es f a
    -> UnionH (Take offset es ++ (UnionH (Take len (Drop offset es)) ': Drop len (Drop offset es))) f a
bundleUnionNUnderH u@(UnionH n a koi)
    | n < off = coerce u
    | n' < len = UnionH 0 (UnionH n' a koi) id
    | otherwise = UnionH (n - len + 1) a koi
  where
    off = wordVal @offset
    len = wordVal @len
    n' = n - off
{-# INLINE bundleUnionNUnderH #-}

unbundleUnionUnderH
    :: forall offset bundle es es' f a
     . (BundleUnder UnionH offset es es' bundle)
    => UnionH es' f a
    -> UnionH es f a
unbundleUnionUnderH = unbundleUnionNUnderH @(Length bundle) @offset
{-# INLINE unbundleUnionUnderH #-}

unbundleUnionNUnderH
    :: forall len offset es f a
     . (KnownNat len, KnownNat offset)
    => UnionH (Take offset es ++ (UnionH (Take len (Drop offset es)) ': Drop len (Drop offset es))) f a
    -> UnionH es f a
unbundleUnionNUnderH u@(UnionH n a koi)
    | n < off = coerce u
    | n == off =
        case unsafeCoerce a of
            UnionH n' a' koi' -> UnionH (off + n') a' (koi . koi')
    | otherwise = UnionH (n - 1 + len) a koi
  where
    off = wordVal @offset
    len = wordVal @len
{-# INLINE unbundleUnionNUnderH #-}

bundleAllUnionH :: UnionH es f a -> UnionH '[UnionH es] f a
bundleAllUnionH u = UnionH 0 u id
{-# INLINE bundleAllUnionH #-}

unbundleAllUnionH :: (HFunctors es) => UnionH '[UnionH es] f a -> UnionH es f a
unbundleAllUnionH = extractH
{-# INLINE unbundleAllUnionH #-}

prefixUnionH :: forall any es f a. (KnownLength any) => UnionH es f a -> UnionH (any ++ es) f a
prefixUnionH (UnionH n a koi) = UnionH (n + reifyLength @any) a koi
{-# INLINE prefixUnionH #-}

prefixUnionUnderH
    :: forall any offset es f a
     . (KnownLength any, KnownNat offset)
    => UnionH es f a
    -> UnionH (Take offset es ++ any ++ Drop offset es) f a
prefixUnionUnderH u@(UnionH n a koi)
    | n < wordVal @offset = coerce u
    | otherwise = UnionH (n + reifyLength @any) a koi
{-# INLINE prefixUnionUnderH #-}

suffixUnionH :: forall any es f a. UnionH es f a -> UnionH (es ++ any) f a
suffixUnionH = coerce
{-# INLINE suffixUnionH #-}

suffixUnionOverNH
    :: forall any offset es f a
     . (KnownLength any, KnownNat offset, KnownLength es)
    => UnionH es f a
    -> UnionH (Take (Length es - offset) es ++ any ++ Drop (Length es - offset) es) f a
suffixUnionOverNH u@(UnionH n a koi)
    | n < reifyLength @es - wordVal @offset = coerce u
    | otherwise = UnionH (n + reifyLength @any) a koi
{-# INLINE suffixUnionOverNH #-}

flipAllUnionH :: forall es f a. (KnownLength es) => UnionH es f a -> UnionH (Reverse es) f a
flipAllUnionH (UnionH n a koi) = UnionH (reifyLength @es - n) a koi
{-# INLINE flipAllUnionH #-}

flipUnionH
    :: forall len es f a
     . (KnownNat len)
    => UnionH es f a
    -> UnionH (Reverse (Take len es) ++ Drop len es) f a
flipUnionH u@(UnionH n a koi)
    | n < len = UnionH (len - n) a koi
    | otherwise = coerce u
  where
    len = wordVal @len
{-# INLINE flipUnionH #-}

flipUnionUnderH
    :: forall len offset es f a
     . (KnownNat len, KnownNat offset)
    => UnionH es f a
    -> UnionH (Take offset es ++ Reverse (Take len (Drop offset es)) ++ Drop len (Drop offset es)) f a
flipUnionUnderH u@(UnionH n a koi)
    | n >= off && n' < len = UnionH (off + len - n') a koi
    | otherwise = coerce u
  where
    off = wordVal @offset
    len = wordVal @len
    n' = n - off
{-# INLINE flipUnionUnderH #-}

nilH :: UnionH '[] f a -> r
nilH _ = error "Effect system internal error: nilH - An empty effect union, which should not be possible to create, has been created."
