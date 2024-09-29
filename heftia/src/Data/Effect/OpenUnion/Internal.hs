-- SPDX-License-Identifier: MPL-2.0 AND BSD-3-Clause
{-# LANGUAGE AllowAmbiguousTypes #-}

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
-}
module Data.Effect.OpenUnion.Internal where

import GHC.TypeError (ErrorMessage (ShowType, Text, (:$$:), (:<>:)), TypeError)

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

class IsSuffixOf es es' where
    prefixLen :: Word

instance IsSuffixOf es es where
    prefixLen = 0

instance {-# OVERLAPPABLE #-} (IsSuffixOf es es') => IsSuffixOf es (e ': es') where
    prefixLen = 1 + prefixLen @es @es'

class KnownLen (es :: [k]) where
    -- | Get the length of the list.
    reifyLen :: Word

instance KnownLen '[] where
    reifyLen = 0

instance (KnownLen es) => KnownLen (e ': es) where
    reifyLen = 1 + reifyLen @_ @es

infixr 5 ++
type family xs ++ ys where
    '[] ++ ys = ys
    (x ': xs) ++ ys = x ': (xs ++ ys)
