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
Stability   :  experimental
Description :  Open unions (type-indexed co-products) for extensible higher-order effects.

Implementation of an open union for higher-order effects.

Importing this module allows unsafe access to the data structure of the open
union, so it should not usually be imported directly.

Based on [the open union in freer-simple](https://hackage.haskell.org/package/freer-simple-1.2.1.2/docs/Data-OpenUnion-Internal.html).
-}
module Data.Effect.OpenUnion.Internal.HO where

import Control.Effect (type (~>))
import Data.Coerce (coerce)
import Data.Effect.HFunctor (HFunctor, hfmap)
import Data.Effect.OpenUnion.Internal (
    Drop,
    FindElem (elemNo),
    IfNotFound,
    IsSuffixOf,
    KnownLength,
    Length,
    P (unP),
    Reverse,
    Take,
    prefixLen,
    reifyLength,
    wordVal,
    type (++),
 )
import Data.Effect.OpenUnion.Internal.Bundle (Bundle, BundleUnder)
import Data.Effect.OpenUnion.Internal.Strengthen (
    Strengthen,
    StrengthenUnder,
    strengthenUnderMap,
 )
import Data.Effect.OpenUnion.Internal.Weaken (
    Weaken,
    WeakenUnder,
 )
import Data.Kind (Type)
import GHC.TypeNats (KnownNat, type (-))
import Unsafe.Coerce (unsafeCoerce)

-- | Kind of higher-order effects.
type EffectH = (Type -> Type) -> Type -> Type

-- | Open union for higher-order effects. Becomes an instance of 'HFunctor' for free.
data UnionH (es :: [EffectH]) (f :: Type -> Type) (a :: Type) where
    UnionH
        :: {-# UNPACK #-} !Word
        -- ^ A natural number tag to identify the element of the union.
        -> e g a
        -- ^ The data of the higher-order effect that is an element of the union.
        -> (g ~> f)
        -- ^ Continuation of interpretation. Due to this component, this open union becomes a free 'HFunctor', which contributes to performance improvement.
        -> UnionH es f a

hfmapUnion :: (f ~> g) -> UnionH es f a -> UnionH es g a
hfmapUnion phi (UnionH n e koi) = UnionH n e (phi . koi)
{-# INLINE hfmapUnion #-}

instance HFunctor (UnionH es) where
    hfmap f = hfmapUnion f
    {-# INLINE hfmap #-}

unsafeInjH :: Word -> e f a -> UnionH es f a
unsafeInjH n e = UnionH n e id
{-# INLINE unsafeInjH #-}

unsafePrjH :: (HFunctor e) => Word -> UnionH es f a -> Maybe (e f a)
unsafePrjH n (UnionH n' e koi)
    | n == n' = Just (hfmap koi $ unsafeCoerce e)
    | otherwise = Nothing
{-# INLINE unsafePrjH #-}

class (FindElem e es) => MemberH (e :: EffectH) es where
    injH :: e f a -> UnionH es f a
    prjH :: (HFunctor e) => UnionH es f a -> Maybe (e f a)

instance (FindElem e es, IfNotFound e es es) => MemberH e es where
    injH = unsafeInjH $ unP (elemNo :: P e es)
    {-# INLINE injH #-}

    prjH = unsafePrjH $ unP (elemNo :: P e es)
    {-# INLINE prjH #-}

infix 3 <!!
type (<!!) = MemberH

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

extractH :: (HFunctor e) => UnionH '[e] f a -> e f a
extractH (UnionH _ a koi) = hfmap koi $ unsafeCoerce a
{-# INLINE extractH #-}

weakenH :: UnionH es f a -> UnionH (any ': es) f a
weakenH (UnionH n a koi) = UnionH (n + 1) a koi
{-# INLINE weakenH #-}

weakensH :: forall es es' f a. (es `IsSuffixOf` es') => UnionH es f a -> UnionH es' f a
weakensH (UnionH n a koi) = UnionH (n + prefixLen @es @es') a koi
{-# INLINE weakensH #-}

weakenNH :: forall len es es' f a. (Weaken len es es') => UnionH es f a -> UnionH es' f a
weakenNH (UnionH n a koi) = UnionH (n + wordVal @len) a koi
{-# INLINE weakenNH #-}

weakenNUnderMH
    :: forall len offset es es' f a
     . (WeakenUnder len offset es es')
    => UnionH es f a
    -> UnionH es' f a
weakenNUnderMH u@(UnionH n a koi)
    | n < wordVal @offset = coerce u
    | otherwise = UnionH (n + wordVal @len) a koi
{-# INLINE weakenNUnderMH #-}

strengthenNH :: forall len es es' f a. (Strengthen len es es') => UnionH es f a -> UnionH es' f a
strengthenNH (UnionH n a koi) = UnionH (strengthenUnderMap @len @0 @es @es' n) a koi
{-# INLINE strengthenNH #-}

strengthenNUnderMH
    :: forall len offset es es' f a
     . (StrengthenUnder len offset es es')
    => UnionH es f a
    -> UnionH es' f a
strengthenNUnderMH u@(UnionH n a koi)
    | n < off = coerce u
    | otherwise = UnionH (off + strengthenUnderMap @len @offset @es @es' (n - off)) a koi
  where
    off = wordVal @offset
{-# INLINE strengthenNUnderMH #-}

bundleUnionH
    :: forall bundle es rest f a
     . (Bundle es bundle rest)
    => UnionH es f a
    -> UnionH (UnionH bundle ': rest) f a
bundleUnionH (UnionH n a koi)
    | n < len = UnionH 0 (UnionH n a koi) id
    | otherwise = UnionH (n - len + 1) a koi
  where
    len = reifyLength @bundle
{-# INLINE bundleUnionH #-}

unbundleUnionH
    :: forall bundle es rest f a
     . (Bundle es bundle rest)
    => UnionH (UnionH bundle ': rest) f a
    -> UnionH es f a
unbundleUnionH (UnionH n a koi)
    | n == 0 = case unsafeCoerce a of
        UnionH n' a' koi' -> UnionH n' a' (koi . koi')
    | otherwise = UnionH (n - 1 + reifyLength @bundle) a koi
{-# INLINE unbundleUnionH #-}

bundleUnionUnderH
    :: forall offset bundle es es' f a
     . (BundleUnder UnionH offset es es' bundle)
    => UnionH es f a
    -> UnionH es' f a
bundleUnionUnderH u@(UnionH n a koi)
    | n < off = coerce u
    | n' < len = UnionH 0 (UnionH n' a koi) id
    | otherwise = UnionH (n - len + 1) a koi
  where
    off = wordVal @offset
    len = reifyLength @bundle
    n' = n - off
{-# INLINE bundleUnionUnderH #-}

unbundleUnionUnderH
    :: forall offset bundle es es' f a
     . (BundleUnder UnionH offset es es' bundle)
    => UnionH es' f a
    -> UnionH es f a
unbundleUnionUnderH u@(UnionH n a koi)
    | n < off = coerce u
    | n == off =
        case unsafeCoerce a of
            UnionH n' a' koi' -> UnionH (off + n') a' (koi . koi')
    | otherwise = UnionH (n - 1 + len) a koi
  where
    off = wordVal @offset
    len = reifyLength @bundle
{-# INLINE unbundleUnionUnderH #-}

bundleAllUnionH :: UnionH es f a -> UnionH '[UnionH es] f a
bundleAllUnionH u = UnionH 0 u id
{-# INLINE bundleAllUnionH #-}

unbundleAllUnionH :: UnionH '[UnionH es] f a -> UnionH es f a
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
{-# INLINE nilH #-}
