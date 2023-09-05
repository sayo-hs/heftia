-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at https://mozilla.org/MPL/2.0/.

module Data.Free.Union where

import Control.Effect.Class (Instruction, type (~>))
import Data.Kind (Constraint)

class Union (u :: [Instruction] -> Instruction) where
    type Member u (f :: Instruction) (fs :: [Instruction]) :: Constraint

    inject :: Member u f fs => f a -> u fs a
    project :: Member u f fs => u fs a -> Maybe (f a)

    comp :: Either (f a) (u fs a) -> u (f ': fs) a
    decomp :: u (f ': fs) a -> Either (f a) (u fs a)

    absurdUnion :: u '[] a -> x

    infixr 5 |+|:
    (|+|:) :: (f a -> r) -> (u fs a -> r) -> u (f ': fs) a -> r
    (f |+|: g) u = case decomp u of
        Left x -> f x
        Right x -> g x

    weakenL :: f a -> u (f ': fs) a
    weakenL = comp . Left

    weakenR :: u fs a -> u (f ': fs) a
    weakenR = comp . Right

    {-# INLINE (|+|:) #-}
    {-# INLINE weakenL #-}
    {-# INLINE weakenR #-}

type family IsMember (f :: Instruction) fs where
    IsMember f (f ': fs) = 'True
    IsMember f (_ ': fs) = IsMember f fs
    IsMember _ '[] = 'False

class s <:: t where
    weakenIns :: s ~> t
