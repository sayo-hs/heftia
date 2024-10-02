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

Importing this module allows unsafe access to the data structure of the open
union, so it should not usually be imported directly.

Based on [the open union in freer-simple](https://hackage.haskell.org/package/freer-simple-1.2.1.2/docs/Data-OpenUnion-Internal.html).
-}
module Data.Effect.OpenUnion.Internal.FO where

import Data.Coerce (coerce)
import Data.Effect.Key (type (#>))
import Data.Effect.OpenUnion.Internal (
    BundleUnder,
    Drop,
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
import GHC.TypeNats (KnownNat, type (-))
import Unsafe.Coerce (unsafeCoerce)

-- | Kind of first-order effects.
type EffectF = Type -> Type

-- | Open union for first-order effects.
data Union (es :: [EffectF]) (a :: Type) where
    Union
        :: {-# UNPACK #-} !Word
        -- ^ A natural number tag to identify the element of the union.
        -> e a
        -- ^ The data of the higher-order effect that is an element of the union.
        -> Union es a

{- | Takes a request of type @e :: 'EffectF'@, and injects it into the 'Union'.

Summand is assigning a specified 'Word' value, which is a position in the
type-list @(e ': es) :: 'EffectF'@.

__This function is unsafe.__

/O(1)/
-}
unsafeInj :: Word -> e a -> Union es a
unsafeInj = Union
{-# INLINE unsafeInj #-}

{- | Project a value of type @'Union' (e ': es) :: 'EffectF'@ into a possible
summand of the type @e :: 'EffectF'@. 'Nothing' means that @e :: 'EffectF'@ is not
the value stored in the @'Union' (e ': es) :: 'EffectF'@.

It is assumed that summand is stored in the 'Union' when the 'Word' value is
the same value as is stored in the 'Union'.

__This function is unsafe.__

/O(1)/
-}
unsafePrj :: Word -> Union es a -> Maybe (e a)
unsafePrj n (Union n' e)
    | n == n' = Just (unsafeCoerce e)
    | otherwise = Nothing
{-# INLINE unsafePrj #-}

{- | A constraint that requires that a particular effect, @e@, is a member of
the type-level list @es@. This is used to parameterize an
'Control.Monad.Hefty.Eff' computation over an arbitrary list of first-order effects, so
long as @e@ is /somewhere/ in the list.

For example, a computation that only needs access to a cell of mutable state
containing an 'Integer' would likely use the following type:

@
'Member' ('Data.Effect.State.State' 'Integer') ef => 'Control.Monad.Hefty.Eff' eh ef ()
@
-}
class (FindElem e es) => Member (e :: EffectF) es where
    -- This type class is used for two following purposes:
    --

    -- * As a @Constraint@ it guarantees that @e :: 'EffectF'@ is a member of a

    --   type-list @es :: ['EffectF']@.
    --

    -- * Provides a way how to inject\/project @e :: 'EffectF'@ into\/from a 'Union',

    --   respectively.
    --
    -- Following law has to hold:
    --
    -- @
    -- 'prj' . 'inj' === 'Just'
    -- @

    -- | Takes a request of type @e :: 'EffectF'@, and injects it into the
    -- 'Union'.
    --
    -- /O(1)/
    inj :: e a -> Union es a

    -- | Project a value of type @'Union' (e ': es) :: 'EffectF'@ into a possible
    -- summand of the type @e :: 'EffectF'@. 'Nothing' means that @e :: 'EffectF'@ is
    -- not the value stored in the @'Union' (e ': es) :: 'EffectF'@.
    --
    -- /O(1)/
    prj :: Union es a -> Maybe (e a)

instance (FindElem e es, IfNotFound e es es) => Member e es where
    inj = unsafeInj $ unP (elemNo :: P e es)
    {-# INLINE inj #-}

    prj = unsafePrj $ unP (elemNo :: P e es)
    {-# INLINE prj #-}

infix 3 <|
type (<|) = Member

type MemberBy key es = Lookup key es <| es

type Lookup key es = Lookup_ key es es

type family Lookup_ (key :: k) r w :: EffectF where
    Lookup_ key (key #> e ': _) w = key #> e
    Lookup_ key (_ ': r) w = Lookup_ key r w
    Lookup_ key '[] w = LookupError key w

{- | Orthogonal decomposition of a @'Union' (e ': es) :: 'EffectF'@. 'Right' value
is returned if the @'Union' (e ': es) :: 'EffectF'@ contains @e :: 'EffectF'@, and
'Left' when it doesn't. Notice that 'Left' value contains
@'Union' es :: 'EffectF'@, i.e. it can not contain @e :: 'EffectF'@.

/O(1)/
-}
decomp :: Union (e ': es) a -> Either (Union es a) (e a)
decomp (Union 0 a) = Right $ unsafeCoerce a
decomp (Union n a) = Left $ Union (n - 1) a
{-# INLINE [2] decomp #-}

{- | Specialized version of 'decomp' for efficiency.

/O(1)/
-}

-- TODO: Check that it actually adds on efficiency.
decomp0 :: Union '[e] a -> Either (Union '[] a) (e a)
decomp0 (Union _ a) = Right $ unsafeCoerce a
{-# INLINE decomp0 #-}

{-# RULES "decomp/singleton" decomp = decomp0 #-}

infixr 5 !+
(!+) :: (e a -> r) -> (Union es a -> r) -> Union (e : es) a -> r
(f !+ g) u = case decomp u of
    Left x -> g x
    Right x -> f x
{-# INLINE (!+) #-}

{- | Specialised version of 'prj'\/'decomp' that works on an
@'Union' '[e] :: 'EffectF'@ which contains only one specific summand. Hence the
absence of 'Maybe', and 'Either'.

/O(1)/
-}
extract :: Union '[e] a -> e a
extract (Union _ a) = unsafeCoerce a
{-# INLINE extract #-}

{- | Inject whole @'Union' es@ into a weaker @'Union' (any ': es)@ that has one
more summand.

/O(1)/
-}
weaken :: forall any es a. Union es a -> Union (any ': es) a
weaken (Union n a) = Union (n + 1) a
{-# INLINE weaken #-}

weakens :: forall es es' a. (es `IsSuffixOf` es') => Union es a -> Union es' a
weakens (Union n a) = Union (n + prefixLen @es @es') a
{-# INLINE weakens #-}

weakenN :: forall len es es' a. (WeakenN len es es') => Union es a -> Union es' a
weakenN (Union n a) = Union (n + wordVal @len) a
{-# INLINE weakenN #-}

weakenUnder :: forall any e es a. Union (e ': es) a -> Union (e ': any ': es) a
weakenUnder u@(Union n a)
    | n == 0 = coerce u
    | otherwise = Union (n + 1) a
{-# INLINE weakenUnder #-}

weakensUnder :: forall offset es es' a. (WeakenUnder offset es es') => Union es a -> Union es' a
weakensUnder = weakenNUnder @(PrefixLength es es') @offset
{-# INLINE weakensUnder #-}

weakenNUnder
    :: forall len offset es es' a
     . (WeakenNUnder len offset es es')
    => Union es a
    -> Union es' a
weakenNUnder u@(Union n a)
    | n < wordVal @offset = coerce u
    | otherwise = Union (n + wordVal @len) a
{-# INLINE weakenNUnder #-}

strengthen :: forall e es a. (e <| es) => Union (e ': es) a -> Union es a
strengthen (Union n a)
    | n == 0 = Union (wordVal @(ElemIndex e es)) a
    | otherwise = Union (n - 1) a
{-# INLINE strengthen #-}

strengthens :: forall es es' a. (Strengthen es es') => Union es a -> Union es' a
strengthens = strengthenN @(PrefixLength es' es)
{-# INLINE strengthens #-}

strengthenN :: forall len es es' a. (StrengthenN len es es') => Union es a -> Union es' a
strengthenN (Union n a) = Union (strengthenMap @_ @_ @len @es @es' n) a
{-# INLINE strengthenN #-}

strengthenUnder :: forall e2 e1 es a. (e2 <| es) => Union (e1 ': e2 ': es) a -> Union (e1 ': es) a
strengthenUnder u@(Union n a)
    | n == 0 = coerce u
    | n == 1 = Union (1 + wordVal @(ElemIndex e2 es)) a
    | otherwise = Union (n - 1) a
{-# INLINE strengthenUnder #-}

strengthensUnder :: forall offset es es' a. (StrengthenUnder offset es es') => Union es a -> Union es' a
strengthensUnder = strengthenNUnder @(PrefixLength es' es) @offset
{-# INLINE strengthensUnder #-}

strengthenNUnder
    :: forall len offset es es' a
     . (StrengthenNUnder len offset es es')
    => Union es a
    -> Union es' a
strengthenNUnder u@(Union n a)
    | n < off = coerce u
    | otherwise = Union (off + strengthenMapUnder @len @offset @es @es' (n - off)) a
  where
    off = wordVal @offset
{-# INLINE strengthenNUnder #-}

bundleUnion
    :: forall es bundle rest a
     . (Split es bundle rest)
    => Union es a
    -> Union (Union bundle ': rest) a
bundleUnion = bundleUnionN @(Length bundle)

bundleUnionN
    :: forall len es a
     . (KnownNat len)
    => Union es a
    -> Union (Union (Take len es) ': Drop len es) a
bundleUnionN (Union n a)
    | n < len = Union 0 $ Union n a
    | otherwise = Union (n - len + 1) a
  where
    len = wordVal @len
{-# INLINE bundleUnionN #-}

unbundleUnion
    :: forall es bundle rest a
     . (Split es bundle rest)
    => Union (Union bundle ': rest) a
    -> Union es a
unbundleUnion = unbundleUnionN @(Length bundle)
{-# INLINE unbundleUnion #-}

unbundleUnionN
    :: forall len es a
     . (KnownNat len)
    => Union (Union (Take len es) ': Drop len es) a
    -> Union es a
unbundleUnionN (Union n a)
    | n == 0 = unsafeCoerce a
    | otherwise = Union (n - 1 + wordVal @len) a
{-# INLINE unbundleUnionN #-}

bundleUnionUnder
    :: forall offset bundle es es' a
     . (BundleUnder Union offset es es' bundle)
    => Union es a
    -> Union es' a
bundleUnionUnder = bundleUnionNUnder @(Length bundle) @offset
{-# INLINE bundleUnionUnder #-}

bundleUnionNUnder
    :: forall len offset es a
     . (KnownNat len, KnownNat offset)
    => Union es a
    -> Union (Take offset es ++ (Union (Take len (Drop offset es)) ': Drop len (Drop offset es))) a
bundleUnionNUnder u@(Union n a)
    | n < off = coerce u
    | n' < len = Union 0 $ Union n' a
    | otherwise = Union (n - len + 1) a
  where
    off = wordVal @offset
    len = wordVal @len
    n' = n - off
{-# INLINE bundleUnionNUnder #-}

unbundleUnionUnder
    :: forall offset bundle es es' a
     . (BundleUnder Union offset es es' bundle)
    => Union es' a
    -> Union es a
unbundleUnionUnder = unbundleUnionNUnder @(Length bundle) @offset
{-# INLINE unbundleUnionUnder #-}

unbundleUnionNUnder
    :: forall len offset es a
     . (KnownNat len, KnownNat offset)
    => Union (Take offset es ++ (Union (Take len (Drop offset es)) ': Drop len (Drop offset es))) a
    -> Union es a
unbundleUnionNUnder u@(Union n a)
    | n < off = coerce u
    | n == off =
        case unsafeCoerce a of
            Union n' a' -> Union (off + n') a'
    | otherwise = Union (n - 1 + len) a
  where
    off = wordVal @offset
    len = wordVal @len
{-# INLINE unbundleUnionNUnder #-}

bundleAllUnion :: Union es a -> Union '[Union es] a
bundleAllUnion = Union 0
{-# INLINE bundleAllUnion #-}

unbundleAllUnion :: Union '[Union es] a -> Union es a
unbundleAllUnion = extract
{-# INLINE unbundleAllUnion #-}

prefixUnion :: forall any es a. (KnownLength any) => Union es a -> Union (any ++ es) a
prefixUnion (Union n a) = Union (n + reifyLength @any) a
{-# INLINE prefixUnion #-}

prefixUnionUnder
    :: forall any offset es a
     . (KnownLength any, KnownNat offset)
    => Union es a
    -> Union (Take offset es ++ any ++ Drop offset es) a
prefixUnionUnder u@(Union n a)
    | n < wordVal @offset = coerce u
    | otherwise = Union (n + reifyLength @any) a
{-# INLINE prefixUnionUnder #-}

suffixUnion :: forall any es a. Union es a -> Union (es ++ any) a
suffixUnion = coerce
{-# INLINE suffixUnion #-}

suffixUnionOverN
    :: forall any offset es a
     . (KnownLength any, KnownNat offset, KnownLength es)
    => Union es a
    -> Union (Take (Length es - offset) es ++ any ++ Drop (Length es - offset) es) a
suffixUnionOverN u@(Union n a)
    | n < reifyLength @es - wordVal @offset = coerce u
    | otherwise = Union (n + reifyLength @any) a
{-# INLINE suffixUnionOverN #-}

flipAllUnion :: forall es a. (KnownLength es) => Union es a -> Union (Reverse es) a
flipAllUnion (Union n a) = Union (reifyLength @es - n) a
{-# INLINE flipAllUnion #-}

flipUnion
    :: forall len es a
     . (KnownNat len)
    => Union es a
    -> Union (Reverse (Take len es) ++ Drop len es) a
flipUnion u@(Union n a)
    | n < len = Union (len - n) a
    | otherwise = coerce u
  where
    len = wordVal @len
{-# INLINE flipUnion #-}

flipUnionUnder
    :: forall len offset es a
     . (KnownNat len, KnownNat offset)
    => Union es a
    -> Union (Take offset es ++ Reverse (Take len (Drop offset es)) ++ Drop len (Drop offset es)) a
flipUnionUnder u@(Union n a)
    | n >= off && n' < len = Union (off + len - n') a
    | otherwise = coerce u
  where
    off = wordVal @offset
    len = wordVal @len
    n' = n - off
{-# INLINE flipUnionUnder #-}

nil :: Union '[] a -> r
nil _ = error "Effect system internal error: nil - An empty effect union, which should not be possible to create, has been created."
