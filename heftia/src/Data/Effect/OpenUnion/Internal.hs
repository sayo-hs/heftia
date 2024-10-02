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
Description :  Open unions (type-indexed co-products) for extensible effects.
-}
module Data.Effect.OpenUnion.Internal where

import Data.Kind (Constraint)
import Data.Proxy (Proxy (Proxy))
import Data.Type.Equality (type (==))
import GHC.TypeError (ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)
import GHC.TypeNats (KnownNat, natVal, type (+), type (-))

{- |
Represents position of element @e :: k@ in a type list @es :: [k]@.
-}
newtype P (e :: k) (es :: [k]) = P {unP :: Word}

{- |
Find an index of an element @e :: k@ in a type list @es :: [k]@.
The element must exist. The @es :: [k]@ type represents the entire list,
prior to recursion, and it is used to produce better type errors.

This is essentially a compile-time computation without run-time overhead.
-}
class FindElem (e :: k) (es :: [k]) where
    -- | Position of the element @e :: k@ in a type list @es :: [k]@.
    --
    -- Position is computed during compilation, i.e. there is no run-time
    -- overhead.
    --
    -- /O(1)/
    elemNo :: P e es

-- | Base case; element is at the current position in the list.
instance FindElem e (e ': es) where
    elemNo = P 0

{- | Recursion; element is not at the current position, but is somewhere in the
list.
-}
instance {-# OVERLAPPABLE #-} (FindElem e es) => FindElem e (e' ': es) where
    elemNo = P $ 1 + unP (elemNo :: P e es)

{- | Instance resolution for this class fails with a custom type error
if @e :: k@ is not in the list @w :: [k]@.
-}
class IfNotFound (e :: k) (r :: [k]) (w :: [k])

{- | If we reach an empty list, that’s a failure, since it means the type isn’t
in the list.
-}
instance
    ( TypeError
        ( 'Text "‘"
            ':<>: 'ShowType e
            ':<>: 'Text "’ is not a member of the type-level list"
            ':$$: 'Text "  ‘" ':<>: 'ShowType w ':<>: 'Text "’"
        )
    )
    => IfNotFound e '[] w

instance IfNotFound e (e ': r) w
instance {-# OVERLAPPABLE #-} (IfNotFound e r w) => IfNotFound e (e' ': r) w

{- |
Pass if @r@ is uninstantiated. The incoherence here is safe, since picking
this instance doesn’t cause any variation in behavior, except possibly the
production of an inferior error message. For more information, see
lexi-lambda/freer-simple#3, which describes the motivation in more detail.
-}
instance {-# INCOHERENT #-} IfNotFound e r w

infixr 5 ++
type family xs ++ ys where
    '[] ++ ys = ys
    (x ': xs) ++ ys = x ': (xs ++ ys)

wordVal :: forall n. (KnownNat n) => Word
wordVal = fromIntegral $ natVal @n Proxy
{-# INLINE wordVal #-}

type family Length es where
    Length '[] = 0
    Length (e ': es) = 1 + Length es

class (KnownNat (Length es)) => KnownLength es
instance (KnownNat (Length es)) => KnownLength es

reifyLength :: forall es. (KnownLength es) => Word
reifyLength = wordVal @(Length es)
{-# INLINE reifyLength #-}

type family PrefixLength es es' where
    PrefixLength es es = 0
    PrefixLength es (e ': es') = 1 + PrefixLength es es'

type IsSuffixOf_ es es' = KnownNat (PrefixLength es es')
class (IsSuffixOf_ es es') => IsSuffixOf es es'
instance (IsSuffixOf_ es es') => IsSuffixOf es es'

prefixLen :: forall es es'. (es `IsSuffixOf` es') => Word
prefixLen = wordVal @(PrefixLength es es')
{-# INLINE prefixLen #-}

type WeakenN len es es' = (es ~ Drop len es', KnownNat len)

type WeakenNUnderM len offset es es' =
    (WeakenN len (Drop offset es) (Drop offset es'), KnownNat offset)

type Strengthen es es' =
    (StrengthenMap (PrefixLength es' es) es es', KnownNat (PrefixLength es es'))

type StrengthenN len es es' = (StrengthenMap len es es', KnownNat len)

type StrengthenUnderM offset es es' =
    (StrengthenNUnderM (PrefixLength es' es) offset es es')

type StrengthenNUnderM len offset es es' =
    (StrengthenMap len (Drop offset es) (Drop offset es'), KnownNat len)

type StrengthenMap len es es' = (StrengthenMap_ (len == 0) len es es')

class
    (isLenZero ~ (len == 0)) =>
    StrengthenMap_ isLenZero len (es :: [k]) (es' :: [k])
    where
    strengthenMap :: Word -> Word

instance StrengthenMap_ 'True 0 es es where
    strengthenMap = id
    {-# INLINE strengthenMap #-}

instance
    (StrengthenMap (len - 1) es es', FindElem e es, (len == 0) ~ 'False)
    => StrengthenMap_ 'False len (e ': es) es'
    where
    strengthenMap = \case
        0 -> unP $ elemNo @_ @e @es
        n -> strengthenMap @_ @_ @(len - 1) @es @es' $ n - 1
    {-# INLINE strengthenMap #-}

type BundleUnderM u offset es es' bundle =
    ( es' ~ Take offset es ++ (u bundle ': Drop (Length bundle) (Drop offset es))
    , es ~ Take offset es' ++ bundle ++ Drop 1 (Drop offset es')
    , bundle ~ Take (PrefixLength (Drop offset es) (Drop 1 (Drop offset es'))) (Drop offset es)
    )

type Split es init tail =
    ( es ~ init ++ tail
    , init ~ Take (PrefixLength es tail) es
    , tail ~ Drop (Length init) tail
    )

type family Take n es where
    Take 0 es = '[]
    Take n (e ': es) = e ': Take n es

type family Drop n es where
    Drop 0 es = es
    Drop n (e ': es) = Drop n es

type Reverse es = Reverse_ '[] es

type family Reverse_ acc es where
    Reverse_ acc '[] = acc
    Reverse_ acc (e ': es) = Reverse_ (e ': acc) es
