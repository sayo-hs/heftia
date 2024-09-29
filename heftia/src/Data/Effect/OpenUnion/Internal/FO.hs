{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

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
Description :  Open unions (type-indexed co-products) for extensible first-order effects.

Implementation of an open union for first-order effects.

Note that since this module is internal, the API may not be preserved when the minor version
changes. In other words, this module does not follow the Haskell Package Versioning Policy
specification.

Importing this module allows unsafe access to the data structure of the open union, so it should not
usually be imported.

Based on [the open union in freer-simple](https://hackage.haskell.org/package/freer-simple-1.2.1.2/docs/Data-OpenUnion-Internal.html).
-}
module Data.Effect.OpenUnion.Internal.FO where

import Data.Coerce (coerce)
import Data.Effect.OpenUnion.Internal (
    FindElem (elemNo),
    IfNotFound,
    IsSuffixOf,
    KnownLen,
    P (unP),
    prefixLen,
    reifyLen,
    type (++),
 )
import Data.Effect.OpenUnion.Internal.Strengthen (
    Strengthen,
    StrengthenUnder,
    strengthenMap,
    strengthenUnderMap,
    strengthenUnderOffset,
 )
import Data.Effect.OpenUnion.Internal.Weaken (
    Weaken,
    WeakenUnder,
    weakenLen,
    weakenUnderLen,
    weakenUnderOffset,
 )
import Data.Kind (Type)
import Unsafe.Coerce (unsafeCoerce)

-- | Kind of first-order effects.
type EffectF = Type -> Type

-- | Open union for first-order effects.
data UnionF (es :: [EffectF]) (a :: Type) where
    UnionF
        :: {-# UNPACK #-} !Word
        -- ^ A natural number tag to identify the element of the union.
        -> e a
        -- ^ The data of the higher-order effect that is an element of the union.
        -> UnionF es a

{- | Takes a request of type @e :: 'EffectF'@, and injects it into the 'UnionF'.

Summand is assigning a specified 'Word' value, which is a position in the
type-list @(e ': es) :: 'EffectF'@.

__This function is unsafe.__

/O(1)/
-}
unsafeInjF :: Word -> e a -> UnionF es a
unsafeInjF = UnionF
{-# INLINE unsafeInjF #-}

{- | Project a value of type @'UnionF' (e ': es) :: 'EffectF'@ into a possible
summand of the type @e :: 'EffectF'@. 'Nothing' means that @e :: 'EffectF'@ is not
the value stored in the @'UnionF' (e ': es) :: 'EffectF'@.

It is assumed that summand is stored in the 'Union' when the 'Word' value is
the same value as is stored in the 'UnionF'.

__This function is unsafe.__

/O(1)/
-}
unsafePrjF :: Word -> UnionF es a -> Maybe (e a)
unsafePrjF n (UnionF n' e)
    | n == n' = Just (unsafeCoerce e)
    | otherwise = Nothing
{-# INLINE unsafePrjF #-}

{- | A constraint that requires that a particular effect, @e@, is a member of
the type-level list @es@. This is used to parameterize an
'Control.Monad.Hefty.Eff' computation over an arbitrary list of first-order effects, so
long as @e@ is /somewhere/ in the list.

For example, a computation that only needs access to a cell of mutable state
containing an 'Integer' would likely use the following type:

@
'MemberF' ('Data.Effect.State.State' 'Integer') ef => 'Control.Monad.Hefty.Eff' eh ef ()
@
-}
class (FindElem e es) => MemberF (e :: EffectF) es where
    -- This type class is used for two following purposes:
    --

    -- * As a @Constraint@ it guarantees that @e :: 'EffectF'@ is a member of a

    --   type-list @es :: ['EffectF']@.
    --

    -- * Provides a way how to inject\/project @e :: 'EffectF'@ into\/from a 'UnionF',

    --   respectively.
    --
    -- Following law has to hold:
    --
    -- @
    -- 'prj' . 'inj' === 'Just'
    -- @

    -- | Takes a request of type @e :: 'EffectF'@, and injects it into the
    -- 'UnionF'.
    --
    -- /O(1)/
    injF :: e a -> UnionF es a

    -- | Project a value of type @'UnionF' (e ': es) :: 'EffectF'@ into a possible
    -- summand of the type @e :: 'EffectF'@. 'Nothing' means that @e :: 'EffectF'@ is
    -- not the value stored in the @'UnionF' (e ': es) :: 'EffectF'@.
    --
    -- /O(1)/
    prjF :: UnionF es a -> Maybe (e a)

instance (FindElem e es, IfNotFound e es es) => MemberF e es where
    injF = unsafeInjF $ unP (elemNo :: P e es)
    {-# INLINE injF #-}

    prjF = unsafePrjF $ unP (elemNo :: P e es)
    {-# INLINE prjF #-}

{- | Orthogonal decomposition of a @'UnionF' (e ': es) :: 'EffectF'@. 'Right' value
is returned if the @'UnionF' (e ': es) :: 'EffectF'@ contains @e :: 'EffectF'@, and
'Left' when it doesn't. Notice that 'Left' value contains
@U'nionF' es :: 'EffectF'@, i.e. it can not contain @e :: 'EffectF'@.

/O(1)/
-}
decompF :: UnionF (e ': es) a -> Either (UnionF r a) (t a)
decompF (UnionF 0 a) = Right $ unsafeCoerce a
decompF (UnionF n a) = Left $ UnionF (n - 1) a
{-# INLINE [2] decompF #-}

{- | Specialized version of 'decomp' for efficiency.

/O(1)/

TODO: Check that it actually adds on efficiency.
-}
decomp0F :: UnionF '[t] a -> Either (UnionF '[] a) (t a)
decomp0F (UnionF _ a) = Right $ unsafeCoerce a
{-# INLINE decomp0F #-}

{-# RULES "decomp/singleton" decompF = decomp0F #-}

{- | Specialised version of 'prj'\/'decomp' that works on an
@'UnionF' '[e] :: 'EffectF'@ which contains only one specific summand. Hence the
absence of 'Maybe', and 'Either'.

/O(1)/
-}
extractF :: UnionF '[e] a -> e a
extractF (UnionF _ a) = unsafeCoerce a
{-# INLINE extractF #-}

{- | Inject whole @'UnionF' es@ into a weaker @'UnionF' (any ': es)@ that has one
more summand.

/O(1)/
-}
weakenF :: UnionF es a -> UnionF (any ': es) a
weakenF (UnionF n a) = UnionF (n + 1) a
{-# INLINE weakenF #-}

weakensF :: forall es es' a. (es `IsSuffixOf` es') => UnionF es a -> UnionF es' a
weakensF (UnionF n a) = UnionF (n + prefixLen @es @es') a
{-# INLINE weakensF #-}

weakenNF :: forall len es es' a. (Weaken len es es') => UnionF es a -> UnionF es' a
weakenNF (UnionF n a) = UnionF (n + weakenLen @len @es @es') a
{-# INLINE weakenNF #-}

weakenNUnderMF
    :: forall len offset es es' a
     . (WeakenUnder len offset es es')
    => UnionF es a
    -> UnionF es' a
weakenNUnderMF u@(UnionF n a)
    | n < weakenUnderOffset @len @offset @es @es' = coerce u
    | otherwise = UnionF (n + weakenUnderLen @len @offset @es @es') a
{-# INLINE weakenNUnderMF #-}

strengthenNF :: forall len es es' a. (Strengthen len es es') => UnionF es a -> UnionF es' a
strengthenNF (UnionF n a) = UnionF (strengthenMap @len @es @es' n) a
{-# INLINE strengthenNF #-}

strengthenNUnderMF
    :: forall len offset es es' a
     . (StrengthenUnder len offset es es')
    => UnionF es a
    -> UnionF es' a
strengthenNUnderMF u@(UnionF n a)
    | n < off = coerce u
    | otherwise = UnionF (off + strengthenUnderMap @len @offset @es @es' (n - off)) a
  where
    off = strengthenUnderOffset @len @offset @es @es'
{-# INLINE strengthenNUnderMF #-}

prefixF :: forall any es a. (KnownLen any) => UnionF es a -> UnionF (any ++ es) a
prefixF (UnionF n a) = UnionF (n + reifyLen @_ @any) a
{-# INLINE prefixF #-}

suffixF :: forall any es a. UnionF es a -> UnionF (es ++ any) a
suffixF = coerce
{-# INLINE suffixF #-}

exhaustF :: UnionF '[] a -> r
exhaustF _ = error "Effect system internal error: exhaustF - An empty effect union, which should not be possible to create, has been created."
{-# INLINE exhaustF #-}
